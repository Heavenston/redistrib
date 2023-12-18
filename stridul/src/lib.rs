#![feature(int_roundings)]
#![feature(maybe_uninit_slice)]

#![allow(dead_code)]

mod socket;
pub mod packet;
pub(crate) use packet::*;

pub mod read_ext;
pub(crate) use read_ext::*;

pub use socket::*;
mod stream;
pub use stream::*;
mod sparious_buffer;
pub(crate) use sparious_buffer::*;

mod packet_stream;
pub use packet_stream::*;

#[cfg(test)]
mod tests;

use std::{net::SocketAddr, time::Duration, hash::Hash, fmt::Debug, io::{Write, Read}};

use bytes::BufMut;
use tokio::net::UdpSocket;
use thiserror::Error;

/// Stable version of the std function
pub(crate) fn default<T: Default>() -> T {
    return T::default();
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    OtherIoError(#[from] tokio::io::Error),
    #[error("Unexpected value {message}")]
    UnexpectedValueError {
        message: String,
    },
    #[error("Tried to send a packet of size {size} but maximum is {maximum}")]
    PacketTooBig {
        size: usize,
        maximum: usize,
    },
    #[error("The driver associated with this socket has been dropped")]
    DriverDropped,

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[async_trait::async_trait]
pub trait StrategySocket<Strat: Strategy>: Send + Sync + 'static {
    fn local_addr(&self) -> Strat::PeersAddr;

    async fn send_to(
        &self, bytes: &[u8], target: &Strat::PeersAddr
    ) -> Result<usize, Error>;

    async fn recv_from(
        &self, bytes: &mut [u8]
    ) -> Result<(usize, Strat::PeersAddr), Error>;
}

#[async_trait::async_trait]
impl<Strat> StrategySocket<Strat> for UdpSocket
    where Strat: Strategy<PeersAddr = SocketAddr>
{
    fn local_addr(&self) -> SocketAddr {
        self.local_addr().expect("Local addr fetch fail")
    }

    async fn send_to(
        &self, bytes: &[u8], target: &SocketAddr
    ) -> Result<usize, Error> {
        Ok(UdpSocket::send_to(self, bytes, target).await?)
    }

    async fn recv_from(
        &self, buf: &mut [u8]
    ) -> Result<(usize, SocketAddr), Error> {
        Ok(UdpSocket::recv_from(self, buf).await?)
    }
}

pub trait Strategy: Debug + Send + Sized + 'static {
    type Socket: StrategySocket<Self>;
    type PeersAddr: Debug + Clone + PartialEq + Eq + Hash + Send + Sync + 'static;

    const BASE_WINDOW_SIZE: u32;
    const BASE_RTO: Duration;
    const PACKET_MAX_SIZE: u32;
    const BUFFER_MAX_SIZE: usize;
    const STREAM_0_RESOLVE_TIMEOUT: Duration = Duration::from_millis(500);
    const PACKET_TIMEOUT: Duration = Duration::from_millis(2000);

    fn serialize(
        packet: &packet::Packet, into: impl Write
    ) -> Result<(), Error> {
        packet.serialize(into)
    }
    fn deserialize(
        from: impl Read
    ) -> Result<Packet, Error> {
        Packet::deserialize(from)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DefaultUDPStrategy;
impl Strategy for DefaultUDPStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = 4096;
    const BASE_RTO: Duration = Duration::from_secs(100);
    const PACKET_MAX_SIZE: u32 = 512;
    const BUFFER_MAX_SIZE: usize = 8192;
}
