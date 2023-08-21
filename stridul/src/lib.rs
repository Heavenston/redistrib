#![feature(int_roundings)]
#![feature(io_error_other)]

#![allow(unused_imports)]
#![allow(dead_code)]

mod one_writer_rwlock;
mod socket;
pub use socket::*;
mod stream;
pub use stream::*;

use std::{net::SocketAddr, time::Duration, sync::Arc};

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
