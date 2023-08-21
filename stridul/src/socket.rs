use crate::*;

use std::{
    net::SocketAddr, time::{Duration, Instant}, sync::{Arc, RwLock, Mutex},
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

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub(crate) struct AckPack {
    pub acked_id: PacketId,
    pub window_size: u32,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct DataPack {
    pub id: PacketId,
    pub data: Bytes,
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
pub(crate) struct InFlightPacket {
    pub packet: DataPack,
    pub addr: SocketAddr,
    pub sent_at: Instant,
    pub rto: Duration,
    pub ack_send: oneshot::Sender<()>,
}

#[derive(derivative::Derivative, thiserror::Error)]
#[derivative(Debug)]
pub enum CreateStreamError {
    #[error("A Stream already exists with the same id and peer addr")]
    AlreadyCreated(#[derivative(Debug="ignore")] Arc<StridulStream>),
}

pub struct StridulSocket {
    /// The retransmission timeout
    base_rto: Duration,
    packet_max_size: u32,
    socket: UdpSocket,

    reliable_in_flight: FlumePipe<InFlightPacket>,
    created_streams: FlumePipe<(
        Arc<StridulStream>, oneshot::Sender<Result<(), CreateStreamError>>
    )>,
}

impl StridulSocket {
    pub async fn new(
        addr: impl ToSocketAddrs
    ) -> Result<(Arc<Self>, StridulSocketDriver), StridulError> {
        let socket = UdpSocket::bind(addr).await?;

        let this = Arc::new(Self {
            base_rto: DEFAULT_RTO,
            packet_max_size: 2u32.pow(9), // 512bytes
            socket,

            reliable_in_flight: flume::unbounded(),
            created_streams: flume::unbounded(),
        });
        let driver = StridulSocketDriver::new(Arc::clone(&this));
        Ok((this, driver))
    }

    pub fn listen_addr(&self) -> Result<SocketAddr, StridulError> {
        Ok(self.socket.local_addr()?)
    }


    pub async fn create_stream(
        self: &Arc<Self>, id: StreamID, peer_addr: SocketAddr,
    ) -> Result<Arc<StridulStream>, CreateStreamError> {
        let (rs_send, rs_recv) = oneshot::channel();
        let stream = StridulStream::new(
            id, peer_addr, Arc::clone(self)
        );
        let _ = self.created_streams.0.send((Arc::clone(&stream), rs_send));
        rs_recv.await.unwrap().map(move |_| stream)
    }

    pub async fn get_or_create_stream(
        self: &Arc<Self>, id: StreamID, peer_addr: SocketAddr,
    ) -> Arc<StridulStream> {
        match self.create_stream(id, peer_addr).await {
            Ok(s) | Err(CreateStreamError::AlreadyCreated(s)) => s,
        }
    }

    pub(crate) fn packet_max_size(&self) -> u32 {
        self.packet_max_size
    }

    pub(crate) async fn send_raw(
        &self, dest: &SocketAddr, packet: &StridulPacket
    ) -> Result<(), StridulError> {
        ca::const_assert!(size_of::<u128>() >= size_of::<usize>());
        assert!((packet.data_len() as u128) < (self.packet_max_size as u128));

        // FIXPERF: Memory pool to not alocation each time
        let data = bincode::serialize(packet)?;
        let sent = self.socket.send_to(&data, dest).await?;
        assert_eq!(sent, data.len());
        Ok(())
    }

    pub(crate) async fn send_raw_reliable(
        &self, dest: SocketAddr, packet: DataPack,
    ) -> Result<oneshot::Receiver<()>, StridulError> {
        ca::const_assert!(size_of::<u128>() >= size_of::<usize>());
        assert!((packet.data.len() as u128) < (self.packet_max_size as u128));

        self.send_raw(&dest, &packet.clone().into()).await?;

        let (ack_send, ack_recv) = oneshot::channel();
        self.reliable_in_flight.0.send_async(InFlightPacket {
            packet,
            addr: dest,
            sent_at: Instant::now(),
            // FIXPERF: Rto based on calculated RTT
            rto: self.base_rto,
            ack_send,
        }).await.ok();

        Ok(ack_recv)
    }
}

pub struct StridulSocketDriver {
    socket: Arc<StridulSocket>,

    // FIXPERF: There gotta be a better data structure
    streams: HashMap<SocketAddr, HashMap<StreamID, Arc<StridulStream>>>,

    /// The list of all packets that has not been acknoledged yet
    /// Sorted from oldest to newest
    packets_in_flight: Vec<InFlightPacket>,
    /// List of Acks that had unknown packet ids in case the packet comes
    /// after
    acks_not_processed: Vec<(AckPack, SocketAddr)>,
}

impl StridulSocketDriver {
    fn new(socket: Arc<StridulSocket>) -> Self {
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
    pub async fn drive(&mut self) -> Result<Arc<StridulStream>, StridulError> {
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
                    let streams = self.streams.entry(*stream.peer_addr())
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

                    let mut new_pack = to_retransmit.packet.clone();
                    new_pack.id.retransmission
                        = new_pack.id.retransmission.wrapping_add(1);
                    self.socket.send_raw(
                        &to_retransmit.addr,
                        &new_pack.into()
                    ).await?;
                }

                e = self.socket.socket.recv_from(&mut buffer) => {
                    let (size, addr) = e?;
                    if size > BUFFER_SIZE {
                        log::trace!("Dropped packet from '{addr}' with too big payload, bytes may have been dropped");
                        continue;
                    }

                    let deser_options = bincode::config::DefaultOptions::new()
                        .with_limit(1024);
                    let mut deser = bincode::Deserializer::from_slice(
                        &buffer, deser_options
                    );
                    let Ok(packet) = serde::Deserialize::deserialize(&mut deser)
                        else {
                            log::trace!("Dropped malformed packet from '{addr}'");
                            continue;
                        };

                    match self.packet(addr, packet).await? {
                        ControlFlow::Continue(()) => (),
                        ControlFlow::Break(b) => return Ok(b),
                    }
                }
            }
        }
    }

    async fn packet(
        &mut self, addr: SocketAddr, packet: StridulPacket,
    ) -> Result<ControlFlow<Arc<StridulStream>>, StridulError> {
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

                log::trace!(
                    "Acked packet {:?} after {:?}",
                    flying_packet.packet.id, flying_packet.sent_at.elapsed()
                );
            },
            StridulPacket::Data(pack) => {
                self.socket.send_raw(&addr, &AckPack {
                    acked_id: pack.id,
                    window_size: u32::pow(2, 12) // 4kb
                }.into()).await?;

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

                let accepted = stream.handle_data_pack(pack).await?;

                if !accepted || !is_new_stream {
                    return Ok(ControlFlow::Continue(()));
                }

                return Ok(ControlFlow::Break(stream));               
            },
        }

        Ok(ControlFlow::Continue(()))
    }
}
