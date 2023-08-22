use crate::*;

use std::{
    time::{Duration, Instant}, sync::{Arc, RwLock, Mutex},
    collections::HashMap, ops::ControlFlow, mem::size_of
};

use static_assertions as ca;
use bincode::Options;
use bytes::{Bytes, BytesMut};
use itertools::Itertools;
use tokio::{net::{UdpSocket, ToSocketAddrs}, stream, time as ttime, sync::oneshot};
use thiserror::Error;
use futures::future::Either as fEither;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct PacketId {
    pub stream_id: StreamID,
    pub sequence_number: u32,
    pub retransmission: u8,
}

impl PacketId {
    pub fn with_rt(self, rt: u8) -> Self {
        Self {
            retransmission: rt,
            ..self
        }
    }
}

impl std::fmt::Display for PacketId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(s{}, seq{}, x{})", self.stream_id, self.sequence_number, self.retransmission)
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub(crate) struct AckPack {
    pub acked_id: PacketId,
    pub window_size: u32,
}

impl std::fmt::Display for AckPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ack({}, w {})", self.acked_id, self.window_size)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct DataPack {
    pub id: PacketId,
    pub data: Bytes,
}

impl std::fmt::Display for DataPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Data({}, s {})", self.id, self.data.len())
    }
}

// FIXPERF: Use rkyv ?
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum StridulPacket {
    Ack(AckPack),
    Data(DataPack),
}

impl StridulPacket {
    pub fn id(&self) -> PacketId {
        use StridulPacket::*;
        match self {
            Ack(AckPack { acked_id: id, .. }) |
            Data(DataPack { id, .. }) => *id,
        }
    }

    pub fn id_mut(&mut self) -> &mut PacketId {
        use StridulPacket::*;
        match self {
            Ack(AckPack { acked_id: id, .. }) |
            Data(DataPack { id, .. }) => id,
        }
    }

    pub fn has_data(&self) -> bool {
        match self {
            StridulPacket::Data { .. } => true,
            _ => false,
        }
    }

    pub fn data(&self) -> Option<&Bytes> {
        use StridulPacket::*;
        match self {
            Ack(..)
                => None,
            Data(DataPack { data, .. })
                => Some(data),
        }
    }

    pub fn data_len(&self) -> usize {
        self.data().map(|b| b.len()).unwrap_or(0)
    }
}

impl std::fmt::Display for StridulPacket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StridulPacket::Ack(pack) => write!(f, "{pack}"),
            StridulPacket::Data(pack) => write!(f, "{pack}")
        }
    }
}

impl From<AckPack> for StridulPacket {
    fn from(value: AckPack) -> Self {
        Self::Ack(value)
    }
}
impl From<DataPack> for StridulPacket {
    fn from(value: DataPack) -> Self {
        Self::Data(value)
    }
}

type FlumePipe<T> = (flume::Sender<T>, flume::Receiver<T>);
pub(crate) struct InFlightPacket<Strat: StridulStrategy> {
    pub packet: DataPack,
    pub addr: Strat::PeersAddr,
    pub sent_at: Instant,
    pub rto: Duration,
    pub ack_send: oneshot::Sender<()>,
}

#[derive(derivative::Derivative, thiserror::Error)]
#[derivative(Debug)]
pub enum CreateStreamError<Strat: StridulStrategy> {
    #[error("A Stream already exists with the same id and peer addr")]
    AlreadyCreated(#[derivative(Debug="ignore")] Arc<StridulStream<Strat>>),
}

pub struct StridulSocket<Strat: StridulStrategy> {
    socket: Strat::Socket,

    reliable_in_flight: FlumePipe<InFlightPacket<Strat>>,
    created_streams: FlumePipe<(
        Arc<StridulStream<Strat>>, oneshot::Sender<Result<(), CreateStreamError<Strat>>>
    )>,
}

impl<Strat: StridulStrategy> StridulSocket<Strat> {
    pub fn new(
        socket: Strat::Socket
    ) -> (Arc<Self>, StridulSocketDriver<Strat>) {
        let this = Arc::new(Self {
            socket,

            reliable_in_flight: flume::unbounded(),
            created_streams: flume::unbounded(),
        });
        let driver = StridulSocketDriver::new(Arc::clone(&this));
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
        Strat::serialize(packet, &mut data)?;
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
        self.reliable_in_flight.0.send_async(InFlightPacket {
            packet,
            addr: dest,
            sent_at: Instant::now(),
            // FIXPERF: Rto based on calculated RTT
            rto: Strat::BASE_RTO,
            ack_send,
        }).await.ok();

        Ok(ack_recv)
    }
}

pub struct StridulSocketDriver<Strat: StridulStrategy> {
    socket: Arc<StridulSocket<Strat>>,

    // FIXPERF: There gotta be a better data structure
    streams: HashMap<
        Strat::PeersAddr, HashMap<StreamID, Arc<StridulStream<Strat>>>
    >,

    /// The list of all packets that has not been acknoledged yet
    /// Sorted from oldest to newest
    packets_in_flight: Vec<InFlightPacket<Strat>>,
    /// List of Acks that had unknown packet ids in case the packet comes
    /// after
    acks_not_processed: Vec<(AckPack, Strat::PeersAddr)>,
}

impl<Strat: StridulStrategy> StridulSocketDriver<Strat> {
    fn new(socket: Arc<StridulSocket<Strat>>) -> Self {
        Self {
            socket,

            streams: default(),

            packets_in_flight: default(),
            acks_not_processed: default(),
        }
    }

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

                Ok(p) = self.socket.reliable_in_flight.1.recv_async() => {
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

    pub fn self_driving(mut self) -> tokio::task::JoinHandle<Result<(), StridulError>> {
        tokio::spawn(async move {
            loop {
                self.drive().await?;
            }
        })
    }

    async fn packet(
        &mut self, addr: Strat::PeersAddr, packet: StridulPacket,
    ) -> Result<ControlFlow<Arc<StridulStream<Strat>>>, StridulError> {
        log::trace!("[{:?}] < {}", self.socket.local_addr()?, packet);
        match packet {
            StridulPacket::Ack(pack) => {
                let Some((acked_packet_pos, _)) =
                    self.packets_in_flight.iter().enumerate()
                    .filter(|(_, x)| x.addr == addr)
                    .find(|(_, x)| x.packet.id == pack.acked_id.with_rt(0))
                else {
                    self.acks_not_processed.push((pack, addr));
                    return Ok(ControlFlow::Continue(()));
                };
                // Get the stream of the acked packet to notify it of the
                // acknoledgment
                let stream = self.streams.get(&addr)
                    .and_then(|sts| sts.get(&pack.acked_id.stream_id));
                if let Some(stream) = stream {
                    stream.handle_ack_pack(pack).await?;
                }
                // Remove the packet from the pending-ack list
                let flying_packet =
                    self.packets_in_flight.remove(acked_packet_pos);

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
