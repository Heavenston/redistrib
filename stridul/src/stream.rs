use crate::*;

use std::{net::SocketAddr, time::Duration, sync::{Arc, Mutex, atomic::{AtomicU32, self}}, marker::PhantomData, pin::{Pin, pin}, task::Poll};

use tokio_util::sync::ReusableBoxFuture;
use std::future::Future;
use bytes::{BytesMut, Bytes, BufMut};
use tokio::{net::{UdpSocket, ToSocketAddrs}, io::{AsyncRead, AsyncWrite}, sync::{Notify, futures::Notified, Mutex as AMutex}};
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

#[derive(Debug, Clone, Copy)]
struct FlyingBufferInfo {
    pub sequence: u32,
    pub size: u32,
}

pub struct StridulStream {
    id: StreamID,
    peer_addr: SocketAddr,
    socket: Arc<StridulSocket>,

    received: Mutex<SortedSpariousBuffer>,
    /// Notified when data is available in received
    readable_notify: Notify,

    receiver_window_size: AtomicU32,

    total_sent: AtomicU32,
    // FIXPERF: Use data structures that would require less copies and locks
    write_buffer: AMutex<BytesMut>,
    in_flights: AMutex<Vec<FlyingBufferInfo>>,
}

impl StridulStream {
    /// Send as much bytes as the receiver window allows
    async fn flush_write_buffer(&self) -> Result<(), StridulError> {
        let mut in_flights = self.in_flights.lock().await;
        let mut write_buffer = self.write_buffer.lock().await;

        let flight_size = in_flights.iter()
            .map(|f| f.size)
            .sum::<u32>();
        let sendable_now = self.receiver_window_size
            .load(atomic::Ordering::Relaxed).saturating_sub(flight_size);

        let packet_sizes = self.socket.packet_max_size();
        let packet_count = sendable_now.div_ceil(packet_sizes);
        for _ in 0..packet_count {
            let remaining_len = write_buffer.len();
            let bytes = write_buffer.split_to(
                (packet_sizes as usize).min(remaining_len)
            ).freeze();

            let size = bytes.len() as u32;
            let total_sent = self.total_sent
                .fetch_add(size, atomic::Ordering::Relaxed);

            self.socket.send_raw_reliable(self.peer_addr, DataPack {
                id: PacketId {
                    stream_id: self.id,
                    sequence_number: total_sent,
                    ..default()
                },
                data: bytes,
            }).await?;

            in_flights.push(FlyingBufferInfo {
                sequence: total_sent,
                size
            });
        }

        Ok(())
    }

    async fn raw_write_bytes(
        &self, bytes: &Bytes
    ) -> Result<(), StridulError> {
        self.write_buffer.lock().await.put_slice(bytes);
        self.flush_write_buffer().await?;
        Ok(())
    }

    pub(crate) fn new(
        id: StreamID,
        peer_addr: SocketAddr,
        socket: Arc<StridulSocket>,
    ) -> Arc<Self> {
        Arc::new(Self {
            id,
            peer_addr,
            socket,

            received: default(),
            readable_notify: Notify::new(),

            receiver_window_size: AtomicU32::new(0),

            total_sent: AtomicU32::new(0),
            write_buffer: default(),
            in_flights: default(),
        })
    }

    pub(crate) async fn handle_data_pack(
        &self,
        pack: DataPack,
    ) -> Result<bool, StridulError> {
        let mut received = self.received.lock().unwrap();
        let slt = received.insert(BuffEl {
            start_idx: pack.id.sequence_number.try_into().unwrap(),
            bytes: pack.data.clone(),
        });
        let is_there_data = received.contiguous_len() > 0;
        drop(received);

        if slt != BufferOverlap::None {
            log::trace!("Dropping {slt:?} packet");
            return Ok(false);
        }

        // FIXPERF: Do not re compute the contiguous len?
        if is_there_data {
            // Wake up anyone waiting for data
            self.readable_notify.notify_one();
        }

        Ok(true)
    }

    pub(crate) async fn handle_ack_pack(
        &self, pack: AckPack,
    ) -> Result<(), StridulError> {
        // FIXME: When a window of 0 is sent, other acks that increase it
        //        may be lost, so after some time an empty packet should
        //        be sent.
        self.receiver_window_size.store(
            pack.window_size, atomic::Ordering::Relaxed
        );
        let mut in_flights = self.in_flights.lock().await;
        let ifpos = in_flights.iter()
            .find_position(|fi| fi.sequence == pack.acked_id.sequence_number)
            .map(|x| x.0);
        if let Some(index) = ifpos {
            in_flights.remove(index);
        }
        self.flush_write_buffer().await?;
        Ok(())
    }

    pub fn id(&self) -> StreamID {
        self.id
    }

    pub fn peer_addr(&self) -> &SocketAddr {
        &self.peer_addr
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
