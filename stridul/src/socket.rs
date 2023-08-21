use crate::*;

use std::{net::SocketAddr, time::Duration, sync::{Arc, RwLock}, collections::HashMap};

use bytes::{Bytes, BytesMut};
use tokio::{net::{UdpSocket, ToSocketAddrs}, stream};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct PacketId {
    pub stream_id: StreamID,
    pub sequence_number: u32,
    pub retransmission: u8,
}

// FIXPERF: Use rkyv ?
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum StridulPacket {
    Ack {
        acked_id: PacketId,
    },
    ROPacket {
        id: PacketId,
        data: Bytes,
    },
}

impl StridulPacket {
    pub fn id(&self) -> PacketId {
        use StridulPacket::*;
        match self {
            Ack { acked_id: id } |
            ROPacket { id, .. } => *id,
        }
    }

    pub fn has_data(&self) -> bool {
        match self {
            StridulPacket::ROPacket { .. } => true,
            _ => false,
        }
    }

    pub fn data(&self) -> Option<&Bytes> {
        use StridulPacket::*;
        match self {
            Ack { .. }
                => None,
            ROPacket { data, .. }
                => Some(data),
        }
    }
}

pub struct NewStream {
    pub remote_addr: SocketAddr,
    pub first_message: Bytes,
    pub stream: Arc<StridulStream>,
}

pub struct StridulSocket {
    /// The retransmission timeout
    rto: Duration,
    socket: UdpSocket,

    // FIXPERF: There gotta be a better data structure
    streams: RwLock<HashMap<SocketAddr, 
        HashMap<StreamID, Arc<StridulStream>>
    >>,
}

impl StridulSocket {
    pub async fn new(
        addr: impl ToSocketAddrs
    ) -> Result<Arc<Self>, StridulError> {
        let socket = UdpSocket::bind(addr).await?;

        let this = Arc::new(Self {
            rto: DEFAULT_RTO,
            socket,
            streams: Default::default(),
        });
        Ok(this)
    }

    pub fn listen_addr(&self) -> Result<SocketAddr, StridulError> {
        Ok(self.socket.local_addr()?)
    }

    /// Drives the socket until a new stream is received
    ///
    /// Must be called in a loop as no packet can be received while this is
    /// not running
    pub async fn drive(
        self: &Arc<Self>,
    ) -> Result<NewStream, StridulError> {
        const BUFFER_SIZE: usize = 1024;
        let mut buffer = BytesMut::zeroed(BUFFER_SIZE + 8);

        loop {
            let (size, addr) = self.socket.recv_from(&mut buffer).await?;
            if size > BUFFER_SIZE {
                log::trace!("Dropped packet from '{addr}' with too big payload, bytes may habe been dropped");
                continue;
            }
            let Ok(packet) = bincode::deserialize::<StridulPacket>(&buffer)
                else {
                    log::trace!("Dropped malformed packet from '{addr}'");
                    continue;
                };

            let stream = 'block: {
                let streams = self.streams.read().unwrap();
                let Some(addr_streams) = streams.get(&addr)
                    else { break 'block None };
                addr_streams
                    .get(&packet.id().stream_id).cloned()
                    .map(|s| s)
            };
            let is_new_stream = stream.is_none();

            if is_new_stream && !packet.has_data() {
                log::trace!("Dropped new stream packet with no data");
                continue;
            }
            
            let stream = stream.unwrap_or_else(|| {
                let stream = StridulStream::new(
                    packet.id().stream_id,
                    addr.clone(),
                    Arc::clone(self)
                );

                let mut streams = self.streams.write().unwrap();
                let addr_streams = streams.entry(addr.clone())
                    .or_default();
                addr_streams.insert(packet.id().stream_id, Arc::clone(&stream));

                stream
            });

            stream.handle_packet(&packet).await?;

            if !is_new_stream { continue; }

            return Ok(NewStream {
                remote_addr: addr,
                first_message: packet.data().expect("Impossible path").clone(),
                stream,
            });
        }
    }

    pub(crate) async fn send_raw(
        &self, dest: SocketAddr, packet: &StridulPacket
    ) -> Result<(), StridulError> {
        // FIXPERF: Memory pool to not alocation each time
        let data = bincode::serialize(packet)?;
        self.socket.send_to(&data, &dest).await?;
        Ok(())
    }
}
