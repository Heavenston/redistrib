use crate::*;

use std::{
    time::{Duration, Instant}, sync::{Arc, RwLock, Mutex, Weak},
    collections::HashMap, ops::ControlFlow, mem::size_of, fmt::Display
};

use static_assertions as ca;
use bytes::{Bytes, BytesMut};
use itertools::Itertools;
use tokio::{net::{UdpSocket, ToSocketAddrs}, stream, time as ttime, sync::{mpsc, oneshot}};
use thiserror::Error;
use futures::future::Either as fEither;

type FlumePipe<T> = (flume::Sender<T>, flume::Receiver<T>);

#[derive(Debug)]
pub(crate) struct InFlightPacket<Strat: StridulStrategy> {
    pub packet: DataPack,
    pub addr: Strat::PeersAddr,
    pub sent_at: Instant,
    pub rto: Duration,
    pub ack_send: oneshot::Sender<()>,
}

impl<Strat: StridulStrategy> Display for InFlightPacket<Strat> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InFlightPacket({}, sender {:?}, sent {}ms, rto {}ms)",
            self.packet, self.addr, self.sent_at.elapsed().as_millis(),
            self.rto.as_millis()
        )
    }
}

#[derive(derivative::Derivative, thiserror::Error)]
#[derivative(Debug)]
pub enum CreateStreamError<Strat: StridulStrategy> {
    #[error("A Stream already exists with the same id and peer addr")]
    AlreadyCreated(#[derivative(Debug="ignore")] Arc<StridulStream<Strat>>),
}

pub struct StridulSocket<Strat: StridulStrategy> {
    socket: Strat::Socket,

    async_packet_send_request_send: mpsc::Sender<(Strat::PeersAddr, StridulPacket)>,
    packets_in_flight_send: mpsc::Sender<InFlightPacket<Strat>>,
    created_streams: FlumePipe<(
        Arc<StridulStream<Strat>>, oneshot::Sender<Result<(), CreateStreamError<Strat>>>
    )>,
}

impl<Strat: StridulStrategy> StridulSocket<Strat> {
    pub fn new(
        socket: Strat::Socket
    ) -> (Arc<Self>, StridulSocketDriver<Strat>) {
        let (
            packets_in_flight_send, packets_in_flight_recv
        ) = mpsc::channel(2048);
        let (
            async_packet_send_request_send, async_packet_send_request_recv,
        ) = mpsc::channel(1024);

        let this = Arc::new(Self {
            socket,

            async_packet_send_request_send,
            packets_in_flight_send,
            created_streams: flume::unbounded(),
        });
        let driver = StridulSocketDriver {
            socket: Arc::clone(&this),

            async_packet_send_request_recv,
            packets_in_flight_recv,

            streams: default(),
            packets_in_flight: default(),
            streams_with_window_0: default(),
        };
        (this, driver)
    }

    pub fn local_addr(&self) -> Result<Strat::PeersAddr, StridulError> {
        Ok(self.socket.local_addr()?)
    }

    pub async fn create_stream(
        self: &Arc<Self>, id: StreamID, peer_addr: Strat::PeersAddr,
    ) -> Result<Arc<StridulStream<Strat>>, CreateStreamError<Strat>> {
        let (rs_send, rs_recv) = oneshot::channel();
        let stream = StridulStream::new(
            id, peer_addr, Arc::clone(self)
        );
        let _ = self.created_streams.0.send((Arc::clone(&stream), rs_send));
        rs_recv.await.unwrap().map(move |_| stream)
    }

    pub async fn get_or_create_stream(
        self: &Arc<Self>, id: StreamID, peer_addr: Strat::PeersAddr,
    ) -> Arc<StridulStream<Strat>> {
        match self.create_stream(id, peer_addr).await {
            Ok(s) | Err(CreateStreamError::AlreadyCreated(s)) => s,
        }
    }

    pub(crate) async fn send_raw(
        &self, dest: &Strat::PeersAddr, packet: &StridulPacket
    ) -> Result<(), StridulError> {
        ca::const_assert!(size_of::<u128>() >= size_of::<usize>());
        assert!((packet.data_len() as u128) <= (Strat::PACKET_MAX_SIZE as u128));

        log::trace!("[{:?}] > {} to {:?}", self.socket.local_addr()?, packet, dest);

        // FIXPERF: Memory pool to not alocation each time
        let mut data = BytesMut::new();
        Strat::serialize(packet, &mut (&mut data).writer())?;
        let sent = self.socket.send_to(&data, dest).await?;
        assert_eq!(sent, data.len());
        Ok(())
    }

    pub(crate) async fn send_raw_reliable(
        &self, dest: Strat::PeersAddr, packet: DataPack,
    ) -> Result<oneshot::Receiver<()>, StridulError> {
        ca::const_assert!(size_of::<u128>() >= size_of::<usize>());
        assert!((packet.data.len() as u128) <= (Strat::PACKET_MAX_SIZE as u128));

        self.send_raw(&dest, &packet.clone().into()).await?;

        let (ack_send, ack_recv) = oneshot::channel();
        self.packets_in_flight_send.send(InFlightPacket {
            packet,
            addr: dest,
            sent_at: Instant::now(),
            // FIXPERF: Rto based on calculated RTT
            rto: Strat::BASE_RTO,
            ack_send,
        }).await.ok();

        Ok(ack_recv)
    }

    pub(crate) fn request_async_send_raw(
        &self, dest: Strat::PeersAddr, packet: StridulPacket
    ) -> bool {
        log::trace!(
            "[{:?}] > {} asyncly to {:?}",
            self.socket.local_addr().unwrap(), packet, dest
        );
        self.async_packet_send_request_send.try_send((dest, packet)).is_ok()
    }
}

pub struct StridulSocketDriver<Strat: StridulStrategy> {
    socket: Arc<StridulSocket<Strat>>,

    // FIXPERF: There gotta be a better data structure
    streams: HashMap<
        Strat::PeersAddr, HashMap<StreamID, Arc<StridulStream<Strat>>>
    >,

    async_packet_send_request_recv: mpsc::Receiver<(Strat::PeersAddr, StridulPacket)>,
    packets_in_flight_recv: mpsc::Receiver<InFlightPacket<Strat>>,

    /// The list of all packets that has not been acknoledged yet
    /// Sorted from oldest to newest
    packets_in_flight: Vec<InFlightPacket<Strat>>,

    streams_with_window_0: Vec<(Weak<StridulStream<Strat>>, Instant)>,
}

impl<Strat: StridulStrategy> StridulSocketDriver<Strat> {
    /// Drives the socket until a new stream is received
    ///
    /// Must be called in a loop as no packet can be received while this is
    /// not running
    pub async fn drive(
        &mut self
    ) -> Result<Arc<StridulStream<Strat>>, StridulError> {
        const BUFFER_SIZE: usize = 1024;
        let mut buffer = BytesMut::zeroed(BUFFER_SIZE + 8);

        loop {
            self.remove_timedout_packets();

            // FIXPERF: Make the search better by using the sorted property ?
            //          I think of just early exit when next packets are send
            //          after the current delay
            let to_retransmit = self.packets_in_flight.iter_mut()
                .min_by_key(|nr| nr.sent_at + nr.rto);
            let next_retransmit = match to_retransmit.as_ref().map(|l| &**l) {
                Some(l) => fEither::Left(
                    ttime::sleep_until(
                        ttime::Instant::from_std(l.sent_at + l.rto)
                    )
                ),
                None => fEither::Right(
                    std::future::pending()
                ),
            };

            // Get the next stream with a window of 0 that needs to be resolved
            // Or one about a stream that have been dropped that needs to be
            // removed
            let to_stream_resolve_0 = self.streams_with_window_0.iter()
                .enumerate()
                .map(|(index, (s, a))| {
                    if s.upgrade().is_none() {
                        return (index, s.clone(), Instant::now());
                    }

                    (index, s.clone(), *a + Strat::STREAM_0_RESOLVE_TIMEOUT)
                })
                .min_by_key(|(_, _, a)| *a);
            let next_stream_resolve_0 = match to_stream_resolve_0.as_ref() {
                Some(s) => fEither::Left(
                    ttime::sleep_until(
                        ttime::Instant::from_std(s.2)
                    )
                ),
                None => fEither::Right(
                    std::future::pending()
                ),
            };

            tokio::select! {
                biased;

                Ok((stream, rslt_send)) = self.socket.created_streams.1.recv_async() => {
                    let streams = self.streams.entry(stream.peer_addr().clone())
                        .or_default();
                    match streams.get(&stream.id()) {
                        None => {
                            streams.insert(stream.id(), stream);
                            let _ = rslt_send.send(Ok(()));
                        }
                        Some(s) => {
                            let _ = rslt_send.send(Err(
                                CreateStreamError::AlreadyCreated(Arc::clone(s))
                            ));
                        }
                    }
                }

                Some((addr, packet)) = self.async_packet_send_request_recv.recv() => {
                    self.socket.send_raw(&addr, &packet).await?;
                }

                Some(p) = self.packets_in_flight_recv.recv() => {
                    log::trace!("[{:?}] In flight: {p}", self.socket.local_addr()?);
                    self.packets_in_flight.push(p);
                }
                
                _ = next_retransmit => {
                    let to_retransmit = to_retransmit.unwrap();
                    // Expanential backoff
                    to_retransmit.rto += to_retransmit.rto * 2;
                    to_retransmit.packet.id.retransmission
                        = to_retransmit.packet.id.retransmission.wrapping_add(1);

                    let new_pack = to_retransmit.packet.clone();
                    self.socket.send_raw(
                        &to_retransmit.addr,
                        &new_pack.into()
                    ).await?;
                }

                _ = next_stream_resolve_0 => {
                    let (index, stream, _) = to_stream_resolve_0.unwrap();
                    self.streams_with_window_0.remove(index);
                    let Some(upgraded) = stream.upgrade()
                        else { continue; };
                    self.socket.send_raw(upgraded.peer_addr(), &DataPack {
                        id: PacketId {
                            stream_id: upgraded.id(),
                            sequence_number: 0,
                            retransmission: 0,
                        },
                        data: Bytes::new(),
                    }.into()).await?;
                }

                e = self.socket.socket.recv_from(&mut buffer) => {
                    let (size, addr) = e?;
                    if size > BUFFER_SIZE {
                        log::debug!(
                            "[{:?}] Dropped packet from '{addr:?}' with too big payload, bytes may have been dropped",
                            self.socket.local_addr()
                        );
                        continue;
                    }

                    let packet = match Strat::deserialize(&buffer[..size]) {
                        Ok(p) => p,
                        Err(e) => {
                            log::debug!(
                                "[{:?}] Dropped malformed packet from '{addr:?}' > {e}",
                                self.socket.local_addr()
                            );
                            continue;
                        }
                    };

                    match self.packet(addr, packet).await? {
                        ControlFlow::Continue(()) => (),
                        ControlFlow::Break(b) => return Ok(b),
                    }
                }
            }
        }
    }

    fn remove_timedout_packets(&mut self) {
        let mut i = 0;
        while i < self.packets_in_flight.len() {
            if self.packets_in_flight[i].sent_at.elapsed() > Strat::PACKET_TIMEOUT {
                log::trace!("[{:?}] Timedout {}",
                    self.socket.local_addr(),
                    self.packets_in_flight[i]
                );
                self.packets_in_flight.remove(i);
            }
            else {
                i += 1;
            }
        }
    }

    async fn packet(
        &mut self, addr: Strat::PeersAddr, packet: StridulPacket,
    ) -> Result<ControlFlow<Arc<StridulStream<Strat>>>, StridulError> {
        log::trace!("[{:?}] < {} from {:?}", self.socket.local_addr()?, packet, addr);
        match packet {
            StridulPacket::Ack(pack) => {
                // Get the stream of the acked packet to notify it of the
                // acknoledgment
                let stream = self.streams.get(&addr)
                    .and_then(|sts| sts.get(&pack.acked_id.stream_id));
                if let Some(stream) = stream {
                    stream.handle_ack_pack(pack).await?;

                    if pack.window_size == 0 {
                        let dwgrd = Arc::downgrade(stream);
                        let is_in = self.streams_with_window_0.iter()
                            .any(|(s, _)| s.ptr_eq(&dwgrd));
                        if !is_in {
                            self.streams_with_window_0.push(
                                (Arc::downgrade(stream), Instant::now())
                            );
                        }
                    }
                }

                let Some((flying_packet_pos, flying_packet)) =
                    self.packets_in_flight.iter().enumerate()
                    .filter(|(_, x)| x.addr == addr)
                    .find(|(_, x)| x.packet.id.with_rt(0) == pack.acked_id.with_rt(0))
                    else {
                        return Ok(ControlFlow::Continue(()));
                    };
                log::trace!("[{:?}] Forgetting {flying_packet}", self.socket.local_addr()?);
                // Remove the packet from the pending-ack list
                let flying_packet =
                    self.packets_in_flight.remove(flying_packet_pos);

                if !flying_packet.ack_send.is_closed() {
                    flying_packet.ack_send.send(()).unwrap();
                }
            },
            StridulPacket::Data(pack) => {
                let (is_new_stream, stream) = self.streams.get(&addr)
                    .and_then(|addr_streams|
                        addr_streams
                            .get(&pack.id.stream_id).cloned()
                            .map(|s| (false, s))
                    ).unwrap_or_else(|| {
                        let stream = StridulStream::new(
                            pack.id.stream_id,
                            addr.clone(),
                            Arc::clone(&self.socket)
                        );

                        let addr_streams = self.streams.entry(addr.clone())
                            .or_default();
                        addr_streams.insert(pack.id.stream_id, Arc::clone(&stream));

                        (true, stream)
                    });

                let id = pack.id;
                let accepted = stream.handle_data_pack(pack).await?;

                if let Some(window_size) = accepted {
                    self.socket.send_raw(&addr, &AckPack {
                        acked_id: id,
                        window_size
                    }.into()).await?;
                }

                if accepted.is_none() || !is_new_stream {
                    return Ok(ControlFlow::Continue(()));
                }

                return Ok(ControlFlow::Break(stream));               
            },
        }

        Ok(ControlFlow::Continue(()))
    }
}
