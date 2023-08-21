use crate::{StridulSocket, StridulPacket, StridulError, PacketId};

use std::{net::SocketAddr, time::Duration, sync::{Arc, Mutex}, marker::PhantomData, pin::{Pin, pin}, task::Poll};

use tokio_util::sync::ReusableBoxFuture;
use std::future::Future;
use bytes::{BytesMut, Bytes, BufMut};
use tokio::{net::{UdpSocket, ToSocketAddrs}, io::{AsyncRead, AsyncWrite}, sync::{Notify, futures::Notified}};
use thiserror::Error;
use itertools::Itertools;

pub(crate) type StreamID = u32;

#[derive(Debug, Clone, Default)]
struct BuffEl {
    pub start_idx: usize,
    pub bytes: Bytes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum BufferOverlap {
    /// Buffer is fully covered by other buffers
    Full,
    /// Buffer is partially covered by other buffers
    Partial,
    #[default]
    None,
}

#[derive(Debug, Clone, Default)]
struct SortedSpariousBuffer {
    pub flushed: usize,
    pub els: Vec<BuffEl>,
}
impl SortedSpariousBuffer {
    pub fn insert(&mut self, el: BuffEl) -> BufferOverlap {
        let i = (0usize..self.els.len())
            .find(|&i| self.els[i].start_idx > el.start_idx)
            .unwrap_or(self.els.len());

        // Note: Next one is the current index one because after insertion
        // it will be moved to the right
        let previous = &self.els[i.saturating_sub(1)];
        let next = &self.els[i.clamp(0, self.els.len()-1)];
        if i > 0 {
            let distance = el.start_idx - previous.start_idx;
            // Previous one is overlapping
            if distance < previous.bytes.len() {
                let overlapped_start_bytes = previous.bytes.len() - distance;
                if overlapped_start_bytes >= el.bytes.len() {
                    return BufferOverlap::Full;
                }
                else {
                    return BufferOverlap::Partial;
                }
            }
        }
        if i < self.els.len().saturating_sub(1) {
            let _ = next;
            todo!("Right overlapp")
        }

        self.els.insert(i, el);

        BufferOverlap::None
    }

    pub fn contiguouses<'a>(&'a self) -> impl Iterator<Item = &'a BuffEl> {
        self.els.iter()
            .scan(self.flushed, |s, el| {
                let start = *s;
                *s = s.wrapping_add(el.bytes.len());
                Some((start, el))
            })
            .take_while(|(cumul_idx, el)| {
                el.start_idx == *cumul_idx
            })
            .map(|x| x.1)
    }

    pub fn contiguous_len(&self) -> usize {
        self.contiguouses().count()
    }

    pub fn drain_contiguous(
        &mut self
    ) -> impl Iterator<Item = Bytes> + '_ {
        let len = self.contiguous_len();

        self.flushed += len;
        self.els.drain(..len)
            .map(|el| el.bytes)
    }
}

pub struct StridulStream {
    id: StreamID,
    peer_addr: SocketAddr,
    socket: Arc<StridulSocket>,

    received: Mutex<SortedSpariousBuffer>,
    /// Notified when data is available in received
    readable_notify: Notify,
}

impl StridulStream {
    pub(crate) fn new(
        id: StreamID,
        peer_addr: SocketAddr,
        socket: Arc<StridulSocket>,
    ) -> Arc<Self> {
        Arc::new(Self {
            id,
            peer_addr,
            socket,

            received: Default::default(),
            readable_notify: Notify::new(),
        })
    }

    pub(crate) async fn handle_packet(
        &self,
        id: PacketId,
        data: Bytes,
    ) -> Result<(), StridulError> {
        let mut received = self.received.lock().unwrap();
        let slt = received.insert(
            BuffEl {
                start_idx: id.sequence_number.try_into().unwrap(),
                bytes: data.clone(),
            }
        );
        let is_there_data = received.contiguous_len() > 0;
        drop(received);

        if slt != BufferOverlap::None {
            log::trace!("Dropping {slt:?} packet");
            return Ok(());
        }

        // FIXPERF: Do not re compute the contiguous len?
        if is_there_data {
            // Wake up anyone waiting for data
            self.readable_notify.notify_one();
        }

        Ok(())
    }

    pub fn reader<'a>(self: &'a StridulStream) -> StridulStreamReader<'a> {
        StridulStreamReader::new(self)
    }
}

/// Reader wrapper for a stridul stream
pub struct StridulStreamReader<'a> {
    stream: &'a StridulStream,
    notify: Pin<Box<Notified<'a>>>,
}

impl<'a> StridulStreamReader<'a> {
    fn new(stream: &'a StridulStream) -> Self {
        let mut n = Box::pin(stream.readable_notify.notified());
        n.as_mut().enable();
        Self {
            notify: n,
            stream,
        }
    }
}

impl<'a> AsyncRead for StridulStreamReader<'a> {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        match self.notify.as_mut().poll(cx) {
            Poll::Ready(()) => (),
            Poll::Pending => return Poll::Pending,
        }

        self.stream.received.lock().unwrap()
            .drain_contiguous().for_each(|x| {
                buf.put_slice(&x)
            });

        // Replace the old notify future that is now useless
        // with a new one, reregistering to wait for data
        let mut n = Box::pin(self.stream.readable_notify.notified());
        n.as_mut().enable();
        self.notify = n;

        Poll::Ready(Ok(()))
    }
}
