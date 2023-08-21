#![feature(int_roundings)]
#![feature(async_fn_in_trait)]
#![feature(io_error_other)]

#![allow(unused_imports)]
#![allow(dead_code)]

mod one_writer_rwlock;
mod socket;
pub use socket::*;
mod stream;
pub use stream::*;

use std::{net::SocketAddr, time::Duration, sync::Arc, hash::Hash, fmt::Debug};

use tokio::net::{UdpSocket, ToSocketAddrs};
use thiserror::Error;

pub(crate) const DEFAULT_RTO: Duration = Duration::from_millis(100);

/// Stable version of the std function
pub(crate) fn default<T: Default>() -> T {
    return T::default();
}

#[derive(Error, Debug)]
pub enum StridulError {
    #[error(transparent)]
    OtherIoError(#[from] tokio::io::Error),
    #[error(transparent)]
    BincodeError(#[from] bincode::Error),
}

impl Into<std::io::Error> for StridulError {
    fn into(self) -> std::io::Error {
        match self {
            Self::OtherIoError(io) => io,
            e => std::io::Error::other(e)
        }
    }
}

pub trait StridulStartSocket<Strat: StridulStrategy> {
    fn local_addr(&self) -> Result<Strat::PeersAddr, StridulError>;

    async fn send_to(
        &self, bytes: &[u8], target: &Strat::PeersAddr
    ) -> Result<usize, StridulError>;

    async fn recv_from(
        &self, bytes: &mut [u8]
    ) -> Result<(usize, Strat::PeersAddr), StridulError>;
}

impl<Strat> StridulStartSocket<Strat> for UdpSocket
    where Strat: StridulStrategy<PeersAddr = SocketAddr>
{
    fn local_addr(&self) -> Result<SocketAddr, StridulError> {
        Ok(self.local_addr()?)
    }

    async fn send_to(
        &self, bytes: &[u8], target: &SocketAddr
    ) -> Result<usize, StridulError> {
        Ok(UdpSocket::send_to(self, bytes, target).await?)
    }

    async fn recv_from(
        &self, buf: &mut [u8]
    ) -> Result<(usize, SocketAddr), StridulError> {
        Ok(UdpSocket::recv_from(self, buf).await?)
    }
}

pub trait StridulStrategy: Sized {
    type Socket: StridulStartSocket<Self>;
    type PeersAddr: Debug + Clone + PartialEq + Eq + Hash;

    const BASE_WINDOW_SIZE: u32;
    const BASE_RTO: Duration;
    const PACKET_MAX_SIZE: u32;
}

pub struct StridulUDPStrategy;
impl StridulStrategy for StridulUDPStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = 4096;
    const BASE_RTO: Duration = Duration::from_secs(100);
    const PACKET_MAX_SIZE: u32 = 512;
}
