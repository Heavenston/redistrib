use crate::socket::Socket;

use tokio::net::{UdpSocket, ToSocketAddrs};

// TODO: Make it generic-based to avoid runtime errors
#[derive(Default)]
pub struct NodeBuilder {
    socket: Option<Socket>,
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

    pub fn with_udp_socket(
        &mut self, udp_socket: UdpSocket,
    ) -> &mut Self {
        assert!(
            self.socket.is_none(),
            "you probably didn't mean to overwrite udp socket"
        );
        self.socket = Some(Socket::new(udp_socket));
        self
    }

    pub fn build(&mut self) -> Node {
        Node {
            socket: self.socket.take().expect("Builder not filled"),
        }
    }
}

pub struct Node {
    socket: Socket,
}

impl Node {
    pub fn builder() -> NodeBuilder {
        NodeBuilder::default()
    }
}
