use crate::{StridulSocket, StridulPacket};

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

#[derive(Debug, Clone, Default)]
struct SortedSpariousBuffer {
    pub flushed: usize,
    pub els: Vec<BuffEl>,
}
impl SortedSpariousBuffer {
    pub fn insert(&mut self, el: BuffEl) {
        let index = (0usize..self.els.len())
            .find(|&i| self.els[i].start_idx >= el.start_idx)
            .unwrap_or(self.els.len());
        self.els.insert(index, el);
    }

    pub fn contiguouses<'a>(&'a self) -> impl Iterator<Item = &'a BuffEl> {
        self.els.iter()
            .scan(0usize, |s, el| {
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
    socket: Arc<StridulSocket>,

    received: Mutex<SortedSpariousBuffer>,
    readable_notify: Notify,
}

impl StridulStream {
    pub(crate) fn new(
        id: StreamID,
        socket: Arc<StridulSocket>,
    ) -> Arc<Self> {
        Arc::new(Self {
            id,
            socket,

            received: Default::default(),
            readable_notify: Notify::new(),
        })
    }

    pub(crate) fn handle_packet(&self, packet: &StridulPacket) {
        use StridulPacket::*;
        match packet {
            Ack { .. } => (),
            ROPacket { id, data } => self.received.lock().unwrap().insert(
                BuffEl {
                    start_idx: id.sequence_number.try_into().unwrap(),
                    bytes: data.clone(),
                }
            ),
        }
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

        let mut n = Box::pin(self.stream.readable_notify.notified());
        n.as_mut().enable();
        self.notify = n;

        Poll::Ready(Ok(()))
    }
}
