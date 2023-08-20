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

pub const DEFAULT_RTO: Duration = Duration::from_millis(100);

#[derive(Error, Debug)]
pub enum StridulError {
    #[error(transparent)]
    OtherIoError(#[from] tokio::io::Error),
    #[error(transparent)]
    BincodeError(#[from] bincode::Error),
}
