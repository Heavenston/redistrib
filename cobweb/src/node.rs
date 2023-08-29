use std::{net::SocketAddr, sync::Arc};

use tokio::net::{UdpSocket, ToSocketAddrs};

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

// TODO: Make it generic-based to avoid runtime errors on build
#[derive(Default)]
pub struct NodeBuilder {
    socket: Option<Arc<stridul::Socket<CobwebStridulStrategy>>>,
    socket_driver: Option<stridul::SocketDriver<CobwebStridulStrategy>>,
}

impl NodeBuilder {
    pub fn new() -> Self {
        NodeBuilder::default()
    }

    pub async fn bind(
        &mut self, addr: impl ToSocketAddrs
    ) -> anyhow::Result<&mut Self> {
        let udp_socket = UdpSocket::bind(addr).await?;
        self.with_udp_socket(udp_socket);
        Ok(self)
    }

    pub fn with_stridul_socket(
        &mut self,
        socket: Arc<stridul::Socket<CobwebStridulStrategy>>,
        socket_driver: stridul::SocketDriver<CobwebStridulStrategy>,
    ) -> &mut Self {
        self.socket = Some(socket);
        self.socket_driver = Some(socket_driver);
        self
    }

    pub fn with_udp_socket(
        &mut self, udp_socket: UdpSocket,
    ) -> &mut Self {
        let (socket, driver) = stridul::Socket::new(udp_socket);
        self.socket = Some(socket);
        self.socket_driver = Some(driver);
        self
    }

    pub fn build(&mut self) -> Node {
        Node {
            socket: self.socket.take().expect("Unfinished builder")
        }
    }
}

pub struct Node {
    socket: Arc<stridul::Socket<CobwebStridulStrategy>>,
}

impl Node {
    pub fn builder() -> NodeBuilder {
        NodeBuilder::default()
    }
}