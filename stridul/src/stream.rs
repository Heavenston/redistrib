use crate::*;

use std::{sync::{Arc, Mutex, atomic::{AtomicU32, self}}, pin::Pin, task::Poll};
use std::future::Future;

use bytes::{BytesMut, BufMut};
use tokio::{io::{AsyncRead, AsyncWrite}, sync::{Notify, futures::Notified, Mutex as AMutex, MutexGuard as AMutexGuard}};
use itertools::Itertools;
use derivative::Derivative;

pub(crate) type StreamID = u32;

#[derive(Debug, Clone, Copy)]
struct FlyingBufferInfo {
    pub sequence: u32,
    pub size: u32,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Stream<Strat: Strategy> {
    id: StreamID,
    peer_addr: Strat::PeersAddr,
    #[derivative(Debug="ignore")]
    socket: Arc<Socket<Strat>>,

    received: Mutex<SortedSpariousBuffer>,
    /// Notified when data is available in received
    readable_notify: Notify,

    last_window_size: AtomicU32,
    receiver_window_size: AtomicU32,

    total_sent: AtomicU32,
    // FIXPERF: Use data structures that would require less copies and locks
    write_buffer: AMutex<BytesMut>,
    in_flights: AMutex<Vec<FlyingBufferInfo>>,
}

impl<Strat: Strategy> Stream<Strat> {
    /// Send as much bytes as the receiver window allows
    async fn flush_write_buffer(&self) -> Result<(), Error> {
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
        socket: Arc<Socket<Strat>>,
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
            last_window_size: AtomicU32::new(Strat::BASE_WINDOW_SIZE),

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
    ) -> Result<Option<u32>, Error> {
        let mut received = self.received.lock().unwrap();

        let slt = received.insert(BuffEl {
            start_idx: pack.id.sequence_number.try_into().unwrap(),
            bytes: pack.data.clone(),
        });
        let is_there_data = received.contiguous_len() > 0;
        let remaining_cap: u32 =
            received.remaining_capacity().try_into().unwrap();
        log::trace!("[{:?}][{}] Recevied {}, {}", self.socket.local_addr(), self.id, pack, received);
        drop(received);

        match slt {
            Ok(_) => (),
            // Full overlaps are duplicates which are expected,
            // we still need to Ack them because of,,, numerous situations
            Err(BufferInsertError::FullOverlap) => log::trace!("Duplicate packet, {pack}"),
            Err(BufferInsertError::PartialOverlap) => {
                log::warn!("Unexpected, maybe malicious packet, {pack}");
                return Ok(None);
            },
            Err(BufferInsertError::WouldOverflow) => {
                log::trace!("Dropping would overflow packet, {pack}");
                return Ok(None);
            },
        }

        if is_there_data {
            // Wake up anyone waiting for data
            // FIXPERF: Maybe batch notifies to have bigger read orders
            self.readable_notify.notify_one();
        }
        self.last_window_size.store(remaining_cap, atomic::Ordering::Relaxed);
        Ok(Some(remaining_cap))
    }

    pub(crate) async fn handle_ack_pack(
        &self, pack: AckPack,
    ) -> Result<(), Error> {
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
        let mut received = self.received.lock().unwrap();
        let written = received
            .drain_contiguous(Some(into.remaining_mut()))
            .map(|x| { into.put_slice(&*x); x.len() })
            .sum();
        let remaining: u32 =
            received.remaining_capacity().try_into().unwrap();
        drop(received);

        // Re-advertise the new wider window size if the last one was empty
        // Since there is no other opportunities of sending acks since no
        // packets are now expected from the sender.
        if written > 0 && self.last_window_size.load(atomic::Ordering::Relaxed) == 0 {
            // Note: For if the "async" packet could not be sent the remote
            // will still not have the new window size but in that case he
            // is expected to send a probe-packet after some time to resolve
            // the situation (also applies if this packet is lost)
            let sent = self.socket.request_async_send_raw(
                self.peer_addr.clone(),
                AckPack {
                    acked_id: PacketId {
                        stream_id: self.id,
                        sequence_number: 0,
                        retransmission: 0,
                    },
                    window_size: remaining,
                }.into()
            );
            if sent {
                self.last_window_size.store(remaining, atomic::Ordering::Relaxed);
            }
        }

        written
    }

    pub async fn read(&self, into: &mut impl BufMut) -> usize {
        if !into.has_remaining_mut()
        { return 0; }

        let mut r;
        while { r = self.try_read(into); r } == 0 {
            self.readable_notify.notified().await;
        }
        return r;
    }

    pub async fn write(
        &self, bytes: impl AsRef<[u8]>
    ) -> Result<(), Error> {
        self.write_buffer.lock().await.put_slice(bytes.as_ref());
        Ok(())
    }

    pub async fn flush(&self) -> Result<(), Error> {
        self.flush_write_buffer().await
    }

    pub fn reader<'a>(&'a self) -> StreamReader<'a, Strat> {
        StreamReader::new(self)
    }

    pub fn writer<'a>(&'a self) -> StreamWriter<'a, Strat> {
        StreamWriter::new(self)
    }
}

/// AsyncReader wrapper for a stridul stream
pub struct StreamReader<'a, Strat: Strategy> {
    stream: &'a Stream<Strat>,
    notify: Pin<Box<Notified<'a>>>,
}

impl<'a, Strat: Strategy> StreamReader<'a, Strat> {
    fn new(stream: &'a Stream<Strat>) -> Self {
        let mut n = Box::pin(stream.readable_notify.notified());
        n.as_mut().enable();
        Self {
            notify: n,
            stream,
        }
    }
}

impl<'a, Strat: Strategy> AsyncRead for StreamReader<'a, Strat> {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        if !buf.has_remaining_mut()
        { return Poll::Ready(Ok(())); }

        while self.stream.try_read(buf) == 0 {
            match self.notify.as_mut().poll(cx) {
                Poll::Ready(()) => (),
                Poll::Pending => return Poll::Pending,
            }
            // Replace the old notify future that is now useless
            // with a new one, reregistering to wait for data
            self.notify = Box::pin(self.stream.readable_notify.notified());
        }

        Poll::Ready(Ok(()))
    }
}

/// AsyncWriter wrapper for a stridul stream
pub struct StreamWriter<'a, Strat: Strategy> {
    stream: &'a Stream<Strat>,
    write_buffer_unlock: Option<Pin<Box<dyn 'a + Future<Output = AMutexGuard<'a, BytesMut>>>>>,
    flushing: Option<Pin<Box<dyn 'a + Future<Output = Result<(), Error>>>>>,
}

impl<'a, Strat: Strategy> StreamWriter<'a, Strat> {
    fn new(stream: &'a Stream<Strat>) -> Self {
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

impl<'a, Strat: Strategy> AsyncWrite for StreamWriter<'a, Strat> {
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
