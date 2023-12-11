use std::{net::SocketAddr, sync::{Arc, Weak}};

use bytes::{BytesMut, BufMut};
use tokio::{net::UdpSocket, sync::broadcast};
use rand::prelude::*;

use crate::packet::Packet;

#[derive(Debug, Clone)]
pub struct CobwebStridulStrategy;
impl stridul::Strategy for CobwebStridulStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = stridul::DefaultUDPStrategy::BASE_WINDOW_SIZE;
    const BASE_RTO: std::time::Duration = stridul::DefaultUDPStrategy::BASE_RTO;
    const PACKET_MAX_SIZE: u32 = stridul::DefaultUDPStrategy::PACKET_MAX_SIZE;
    const BUFFER_MAX_SIZE: usize = stridul::DefaultUDPStrategy::BUFFER_MAX_SIZE;
}

pub type SStream = stridul::Stream<CobwebStridulStrategy>;
pub type SSocket = stridul::Socket<CobwebStridulStrategy>;
pub type SDriver = stridul::SocketDriver<CobwebStridulStrategy>;
pub type SPacketStream = stridul::PacketStream<CobwebStridulStrategy>;

#[derive(Debug, Clone)]
pub enum SocketDriveEvent {
    Message {
        from: SocketAddr,
        packet: Arc<Packet>,
    },
    Stream {
        stream: Arc<SStream>,
    },
}

struct SharedSocket {
    event_sender: broadcast::Sender<SocketDriveEvent>,
    ssocket: Arc<SSocket>,
}

pub struct Socket {
    shared: Arc<SharedSocket>,
}

impl Socket {
    /// Creates a new socket from the given udp socket
    pub fn new(udp_socket: UdpSocket) -> Self {
        let (ssocket, driver) = SSocket::new(udp_socket);

        let shared = Arc::new(SharedSocket {
            event_sender: broadcast::channel(1024).0,
            ssocket,
        });

        let sn = Arc::downgrade(&shared);
        tokio::spawn(async move {
            driver_task(sn, driver).await.expect("Driver crashed");
        });

        Self {
            shared,
        }
    }

    pub async fn next_event() {

    }

    pub async fn send_message(
        &self, to: SocketAddr, packet: &Packet,
    ) -> anyhow::Result<()> {
        let mut bytes = BytesMut::new();

        bincode::serialize_into((&mut bytes).writer(), packet)?;

        self.shared.ssocket.send_message(to, bytes.freeze())
            .await?;

        Ok(())
    }

    pub async fn stream_to(
        &self, to: SocketAddr
    ) -> anyhow::Result<SPacketStream> {
        let id = rand::thread_rng().gen();

        let stream = self.shared.ssocket.create_stream(id, to)
            .await?;
        let stream = SPacketStream::create(stream);

        Ok(stream)
    }
}

async fn driver_task(
    shared_node: Weak<SharedSocket>,
    mut docker_driver: SDriver,
) -> anyhow::Result<()> {
    loop {
        let Some(sn) = shared_node.upgrade()
            else { break; };

        let event = docker_driver.drive().await?;

        use stridul::DrivingEvent as DE;
        match event {
            DE::NewStream { stream } => {
                let _ = sn.event_sender.send(
                    SocketDriveEvent::Stream { stream }
                );
            },
            DE::Message { data, from } => {
                let packet = match bincode::deserialize(&data) {
                    Ok(p) => Arc::new(p),
                    Err(e) => {
                        log::warn!("Invalid message from {from} > {e}");
                        continue;
                    },
                };

                let _ = sn.event_sender.send(
                    SocketDriveEvent::Message { from, packet }
                );
            },
        }
    }

    Ok(())
}

