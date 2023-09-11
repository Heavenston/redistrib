use crate::{region::Region, packet::{Packet, PacketKind}};

use std::{net::SocketAddr, sync::{Arc, RwLock, Weak}, collections::HashMap, time::Duration};

use stridul::SocketDriver;
use tokio::{net::{UdpSocket, ToSocketAddrs}, sync::broadcast, time::{error::Elapsed, timeout}};
use rand::prelude::*;

// TODO: Make it generic-based to avoid runtime errors on build
#[derive(Default)]
pub struct NodeBuilder {
    socket: Option<Arc<SSocket>>,
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
            current_region: Default::default(),

            peers: Default::default(),
            parent: Default::default(),
            children: Default::default(),
        }
    }
}

/// Should be running in a tokio task the background to make Node work


#[derive(Debug)]
pub(crate) struct PeerData {
    pub region: Option<Region>,
}

#[derive(Debug)]
pub(crate) struct Peer {
    pub addr: SocketAddr,
    pub data: RwLock<PeerData>,
}

// pub(crate) struct SharedNode {
//     socket: Arc<SSocket>,
//     event_sender: broadcast::Sender<NodeDriveEvent>,
// }

// impl SharedNode {
//     pub async fn wait_for_message(
//         &self,
//         expect_from: Option<SocketAddr>, expect_kind: Option<PacketKind>
//     ) -> Result<Arc<Packet>, Elapsed> {
//         let mut recv = self.event_sender.subscribe();

//         while let Ok(event) = timeout(
//             Duration::from_secs(2000), recv.recv()
//         ).await? {
//             let NodeDriveEvent::Message { from, packet } = event
//                 else { continue; };

//             if expect_from.is_some_and(|s| s != from)
//             { continue; }
//             if expect_kind.is_some_and(|k| k != packet.kind())
//             { continue; }

//             return Ok(packet);
//         }

//         unreachable!("Only on recv error which would mean sender is dropped")
//     }

//     pub async fn create_stream(
//         &self, to: SocketAddr
//     ) -> anyhow::Result<Arc<SStream>> {
//         let stream_id = thread_rng().gen();
//         Ok(self.socket.create_stream(stream_id, to).await?)
//     }

//     pub async fn send_packet(
//         &self, stream: &Arc<SStream>
//     ) -> anyhow::Result<()> {
//         stream.write(bincode::serialize(&Packet::TopLevelRequest)?).await?;
//         stream.flush().await?;
//         Ok(())
//     }
// }

pub struct Node {
    // shared: Arc<SharedNode>,
    current_region: Option<Region>,

    peers: HashMap<SocketAddr, Arc<Peer>>,
    parent: Option<Arc<Peer>>,
    children: [Option<Arc<Peer>>; 2],
}

impl Node {
    pub fn builder() -> NodeBuilder {
        NodeBuilder::default()
    }

    pub async fn bootstrap(
        &mut self,
        addr: SocketAddr,
    ) -> anyhow::Result<()> {

        Ok(())
    }
}