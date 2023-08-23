use crate::*;

use std::{time::Duration, sync::{Arc, Mutex, atomic::{AtomicU32, self}}, marker::PhantomData, pin::{Pin, pin}, task::Poll, io::Write, fmt::Display};

use tokio_util::sync::ReusableBoxFuture;
use std::future::Future;
use bytes::{BytesMut, Bytes, BufMut};
use tokio::{net::{UdpSocket, ToSocketAddrs}, io::{AsyncRead, AsyncWrite}, sync::{Notify, futures::Notified, Mutex as AMutex, MutexGuard as AMutexGuard}};
use thiserror::Error;
use itertools::Itertools;

pub(crate) type StreamID = u32;

#[derive(Debug, Clone, Default)]
struct BuffEl {
    pub start_idx: usize,
    pub bytes: Bytes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BufferInsertError {
    /// Buffer is fully covered by other buffers
    FullOverlap,
    /// Buffer is partially covered by other buffers
    PartialOverlap,
    /// Insertion would make theoriatical size get over maxmimum
    WouldOverflow,
}

#[derive(Debug, Clone)]
struct SortedSpariousBuffer {
    /// May be more than the first element start_idx
    /// This is called over-flush and means that these first
    /// bytes are already flushed.
    pub flushed_bytes: usize,
    pub els: Vec<BuffEl>,
    pub max_size: usize,
}

impl Default for SortedSpariousBuffer {
    fn default() -> Self {
        Self {
            flushed_bytes: default(),
            els: default(),
            max_size: 4096,
        }
    }
}

impl SortedSpariousBuffer {
    pub fn insert(&mut self, el: BuffEl) -> Result<(), BufferInsertError> {
        let i = (0usize..self.els.len())
            .find(|&i| self.els[i].start_idx >= el.start_idx)
            .unwrap_or(self.els.len());

        let new_max_size = 
            self.els.iter()
            .chain([&el])
            .map(|x| x.start_idx + x.bytes.len())
            .max().unwrap_or(0)
            .saturating_sub(self.flushed_bytes);
        if new_max_size > self.max_size {
            return Err(BufferInsertError::WouldOverflow);
        }

        // TODO: Detect partial overlaps
        if let Some(el2) = self.els.get(i) {
            if el2.start_idx == el.start_idx {
                return Err(BufferInsertError::FullOverlap);
            }
        }
        if el.start_idx + el.bytes.len() <= self.flushed_bytes {
            return Err(BufferInsertError::FullOverlap);
        }

        self.els.insert(i, el);

        Ok(())
    }

    pub fn actual_len(&self) -> usize {
        self.els.iter()
            .map(|x| x.bytes.len())
            .sum()
    }

    /// Maximum theratical size if all dis-continuities are filled
    pub fn max_size(&self) -> usize {
        self.els.iter()
            .map(|x| x.start_idx + x.bytes.len())
            .max().unwrap_or(0)
            .saturating_sub(self.flushed_bytes)
    }

    pub fn contiguouses<'a>(&'a self) -> impl Iterator<Item = &'a BuffEl> {
        self.els.iter()
            .scan(self.flushed_bytes, |s, el| {
                let overflushed_bytes = s.saturating_sub(el.start_idx);

                let start = *s;
                *s = s.wrapping_add(el.bytes.len())
                      .wrapping_sub(overflushed_bytes);
                Some((start, el))
            })
            .take_while(|(cumul_idx, el)| {
                el.start_idx <= *cumul_idx
            })
            .map(|x| x.1)
    }

    pub fn contiguous_len(&self) -> usize {
        self.contiguouses().count()
    }

    pub fn contiguous_bytes_len(&self) -> usize {
        self.contiguouses()
            .map(|el| el.bytes.len())
            .sum()
    }

    pub fn drain_contiguous(
        &mut self, max_bytes: Option<usize>,
    ) -> ContiguousFlushIterator<'_> {
        return ContiguousFlushIterator {
            buffer: self,
            max_remaining_bytes: max_bytes,
        };
    }
}

impl Display for SortedSpariousBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SortedSpariousBuffer({}", self.flushed_bytes)?;
        for el in &self.els {
            write!(f, " -> ({}+{}={})", el.start_idx, el.bytes.len(), el.start_idx + el.bytes.len())?;
        }
        write!(f, ")")?;

        Ok(())
    }
}

struct ContiguousFlushIterator<'a> {
    buffer: &'a mut SortedSpariousBuffer,
    max_remaining_bytes: Option<usize>,
}

impl<'a> Iterator for ContiguousFlushIterator<'a> {
    type Item = Bytes;

    fn next(&mut self) -> Option<Self::Item> {
        log::trace!("{}", self.buffer);
        let current = self.buffer.els.get(0)?;
        if current.start_idx > self.buffer.flushed_bytes
        { return None; }

        // Remainning bytes in the current element
        // (less than len in case of over flush)
        let rem_start = self.buffer.flushed_bytes
            .saturating_sub(current.start_idx);
        let rem_in_curr =
            (current.start_idx + current.bytes.len())
            .saturating_sub(self.buffer.flushed_bytes);
        debug_assert_ne!(rem_in_curr, 0, "Element with 0 remaning should be removed");
        log::trace!("First buffer is {} + {} - {}", current.start_idx, current.bytes.len(), self.buffer.flushed_bytes);

        let bytes_to_take = if let Some(max) = self.max_remaining_bytes {
            max.min(rem_in_curr)
        } else {
            rem_in_curr
        };
        log::trace!("Taking {} + {rem_start}..{}", self.buffer.flushed_bytes, rem_start + bytes_to_take);

        if bytes_to_take == 0 { return None; }

        // FIXPERF: Avoid bytes clone if taking all bytes
        //          Saves... picoseconds ?
        let bytes = current.bytes.slice(rem_start..rem_start + bytes_to_take);

        // Remove if fully flushed el
        // FIXPERF: Bulk remove all elements when iterator is dropped
        //          to avoid the movement to the left n times
        if bytes_to_take == rem_in_curr
        { self.buffer.els.remove(0); }

        self.buffer.flushed_bytes += bytes_to_take;

        if let Some(max) = &mut self.max_remaining_bytes {
            *max = max.saturating_sub(bytes_to_take);
        }

        Some(bytes)
    }
}

#[derive(Debug, Clone, Copy)]
struct FlyingBufferInfo {
    pub sequence: u32,
    pub size: u32,
}

pub struct StridulStream<Strat: StridulStrategy> {
    id: StreamID,
    peer_addr: Strat::PeersAddr,
    socket: Arc<StridulSocket<Strat>>,

    received: Mutex<SortedSpariousBuffer>,
    /// Notified when data is available in received
    readable_notify: Notify,

    receiver_window_size: AtomicU32,
    my_window_size: AtomicU32,

    total_sent: AtomicU32,
    // FIXPERF: Use data structures that would require less copies and locks
    write_buffer: AMutex<BytesMut>,
    in_flights: AMutex<Vec<FlyingBufferInfo>>,
}

impl<Strat: StridulStrategy> StridulStream<Strat> {
    /// Send as much bytes as the receiver window allows
    async fn flush_write_buffer(&self) -> Result<(), StridulError> {
        let mut in_flights = self.in_flights.lock().await;
        let mut write_buffer = self.write_buffer.lock().await;

        let flight_size = in_flights.iter()
            .map(|f| f.size)
            .sum::<u32>();
        let sendable_now = self.receiver_window_size
            .load(atomic::Ordering::Relaxed).saturating_sub(flight_size);

        let packet_sizes = Strat::PACKET_MAX_SIZE;
        let packet_count = sendable_now.div_ceil(packet_sizes);
        for _ in 0..packet_count {
            let remaining_len = write_buffer.len();
            if remaining_len == 0
            { break }
            let bytes = write_buffer.split_to(
                (packet_sizes as usize).min(remaining_len)
            ).freeze();

            let size = bytes.len() as u32;
            let total_sent = self.total_sent
                .fetch_add(size, atomic::Ordering::Relaxed);

            self.socket.send_raw_reliable(self.peer_addr.clone(), DataPack {
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

    pub(crate) fn new(
        id: StreamID,
        peer_addr: Strat::PeersAddr,
        socket: Arc<StridulSocket<Strat>>,
    ) -> Arc<Self> {
        Arc::new(Self {
            id,
            peer_addr,
            socket,

            received: Mutex::new(SortedSpariousBuffer {
                max_size: Strat::BUFFER_MAX_SIZE,
                ..default()
            }),
            readable_notify: Notify::new(),

            receiver_window_size: AtomicU32::new(Strat::BASE_WINDOW_SIZE),
            my_window_size: AtomicU32::new(Strat::BASE_WINDOW_SIZE),

            total_sent: AtomicU32::new(0),
            write_buffer: default(),
            in_flights: default(),
        })
    }

    /// Returns Some(window_size) if packet is accepted
    /// Or None if packet is dropped
    pub(crate) async fn handle_data_pack(
        &self,
        pack: DataPack,
    ) -> Result<Option<u32>, StridulError> {
        let mut received = self.received.lock().unwrap();
        let slt = received.insert(BuffEl {
            start_idx: pack.id.sequence_number.try_into().unwrap(),
            bytes: pack.data.clone(),
        });
        let is_there_data = received.contiguous_len() > 0;
        log::trace!("[{:?}][{}] Recevied {}, {}", self.socket.local_addr()?, self.id, pack, received);
        drop(received);

        if let Err(e) = slt {
            log::trace!("Dropping {e:?} packet");
            return Ok(None);
        }

        // FIXPERF: Do not re compute the contiguous len?
        if is_there_data {
            // Wake up anyone waiting for data
            self.readable_notify.notify_one();
        }

        Ok(Some(self.my_window_size.load(atomic::Ordering::Relaxed)))
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
        drop(in_flights);
        self.flush_write_buffer().await?;
        Ok(())
    }

    pub fn id(&self) -> StreamID {
        self.id
    }

    pub fn peer_addr(&self) -> &Strat::PeersAddr {
        &self.peer_addr
    }

    pub fn try_read(&self, into: &mut impl BufMut) -> usize {
        self.received.lock().unwrap()
            .drain_contiguous(Some(into.remaining_mut()))
            .map(|x| { into.put_slice(&*x); x.len() })
            .sum()
    }

    pub async fn read(&self, into: &mut impl BufMut) -> usize {
        let r = self.try_read(into);
        if r != 0 { return r; }
        self.readable_notify.notified().await;
        self.try_read(into)
    }

    pub async fn write(
        &self, bytes: impl AsRef<[u8]>
    ) -> Result<(), StridulError> {
        self.write_buffer.lock().await.put_slice(bytes.as_ref());
        Ok(())
    }

    pub async fn flush(&self) -> Result<(), StridulError> {
        self.flush_write_buffer().await
    }

    pub fn reader<'a>(&'a self) -> StridulStreamReader<'a, Strat> {
        StridulStreamReader::new(self)
    }

    pub fn writer<'a>(&'a self) -> StridulStreamWriter<'a, Strat> {
        StridulStreamWriter::new(self)
    }
}

/// AsyncReader wrapper for a stridul stream
pub struct StridulStreamReader<'a, Strat: StridulStrategy> {
    stream: &'a StridulStream<Strat>,
    notify: Pin<Box<Notified<'a>>>,
}

impl<'a, Strat: StridulStrategy> StridulStreamReader<'a, Strat> {
    fn new(stream: &'a StridulStream<Strat>) -> Self {
        let mut n = Box::pin(stream.readable_notify.notified());
        n.as_mut().enable();
        Self {
            notify: n,
            stream,
        }
    }
}

impl<'a, Strat: StridulStrategy> AsyncRead for StridulStreamReader<'a, Strat> {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        // FIXPERF: Maybe try lock ?
        while self.stream.received.lock().unwrap().contiguous_bytes_len() == 0 {
            match self.notify.as_mut().poll(cx) {
                Poll::Ready(()) => (),
                Poll::Pending => return Poll::Pending,
            }
            // Replace the old notify future that is now useless
            // with a new one, reregistering to wait for data
            let mut n = Box::pin(self.stream.readable_notify.notified());
            n.as_mut().enable();
            self.notify = n;
        }

        self.stream.received.lock().unwrap()
            .drain_contiguous(Some(buf.remaining_mut()))
            .for_each(|x| {
                buf.put_slice(&x)
            });

        Poll::Ready(Ok(()))
    }
}

/// AsyncWriter wrapper for a stridul stream
pub struct StridulStreamWriter<'a, Strat: StridulStrategy> {
    stream: &'a StridulStream<Strat>,
    write_buffer_unlock: Option<Pin<Box<dyn 'a + Future<Output = AMutexGuard<'a, BytesMut>>>>>,
    flushing: Option<Pin<Box<dyn 'a + Future<Output = Result<(), StridulError>>>>>,
}

impl<'a, Strat: StridulStrategy> StridulStreamWriter<'a, Strat> {
    fn new(stream: &'a StridulStream<Strat>) -> Self {
        Self {
            stream,
            write_buffer_unlock: None,
            flushing: None,
        }
    }

    fn create_write_buffer_future(&mut self) {
        if self.write_buffer_unlock.is_some() { return }
        let w = Box::pin(self.stream.write_buffer.lock());
        self.write_buffer_unlock = Some(w as _);
    }

    fn create_flushing_future(&mut self) {
        if self.flushing.is_some() { return }
        let w = Box::pin(self.stream.flush_write_buffer());
        self.flushing = Some(w as _);
    }
}

impl<'a, Strat: StridulStrategy> AsyncWrite for StridulStreamWriter<'a, Strat> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.create_write_buffer_future();
        let f = self.write_buffer_unlock.as_mut().unwrap();
        let mut b = match f.as_mut().poll(cx) {
            Poll::Ready(b) => b,
            Poll::Pending => return Poll::Pending,
        };
        self.write_buffer_unlock = None;
        b.put_slice(buf);
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(
        mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>
    ) -> Poll<Result<(), std::io::Error>> {
        self.create_flushing_future();
        let f = self.flushing.as_mut().unwrap();
        match f.as_mut().poll(cx) {
            Poll::Ready(r) => {
                self.flushing = None;
                Poll::Ready(r.map_err(std::io::Error::other))
            },
            Poll::Pending
                => Poll::Pending,
        }
    }

    fn poll_shutdown(
        self: Pin<&mut Self>, _: &mut std::task::Context<'_>
    ) -> Poll<Result<(), std::io::Error>> {
        Poll::Ready(Ok(()))
    }
}
