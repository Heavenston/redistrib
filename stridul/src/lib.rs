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

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Mutex};

    use bytes::{BytesMut, BufMut, Bytes};
    use tokio::sync::{futures::Notified, Notify};

    use crate::*;

    #[derive(Default)]
    struct AllSockets {
        sockets: Mutex<HashMap<u32, Arc<LocalSocket>>>,
    }

    struct LocalSocket {
        all: Arc<AllSockets>,
        new_data: Notify,
        recv_buffer: Mutex<Vec<(u32, Bytes)>>,
        addr: u32,
    }

    impl LocalSocket {
        fn new(all: &Arc<AllSockets>, addr: u32) -> Arc<Self> {
            let this = Arc::new(Self {
                all: all.clone(),
                new_data: default(),
                recv_buffer: default(),
                addr
            });
            all.sockets.lock().unwrap().insert(addr, this.clone());
            this
        }
    }

    impl StridulStartSocket<LocalStrategy> for Arc<LocalSocket> {
        fn local_addr(&self) -> Result<u32, StridulError> {
            Ok(self.addr)
        }

        async fn send_to(
            &self, bytes: &[u8], target: &u32
        ) -> Result<usize, StridulError> {
            println!("Writing {:?}", bytes);
            let sockets = self.all.sockets.lock().unwrap();
            let t = sockets.get(target).unwrap();
            t.recv_buffer.lock().unwrap().push((self.addr, Bytes::from(bytes.to_vec())));
            t.new_data.notify_one();
            Ok(bytes.len())
        }

        async fn recv_from(
            &self, bytes: &mut [u8]
        ) -> Result<(usize, u32), StridulError> {
            let (sender, data) = loop {
                self.new_data.notified().await;
                if let Some(x) = self.recv_buffer.lock().unwrap().pop()
                { break x };
            };
            let copy_size = bytes.len().min(data.len());
            (&mut bytes[..copy_size]).copy_from_slice(&data[..copy_size]);
            Ok((data.len(), sender))
        }
    }

    struct LocalStrategy;
    impl StridulStrategy for LocalStrategy {
        type Socket = Arc<LocalSocket>;
        type PeersAddr = u32;

        const BASE_WINDOW_SIZE: u32 = 32;
        const BASE_RTO: Duration = Duration::from_millis(1);
        const PACKET_MAX_SIZE: u32 = 8;
    }

    #[tokio::test]
    async fn full_a_to_b() -> Result<(), StridulError> {
        env_logger::builder().is_test(true).try_init().unwrap();

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0);
        let (socket_a, mut socket_a_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1);
        let (socket_b, mut socket_b_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_b);

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr()?)
            .await;
        log::info!("Waiting for write");
        a2b.write(b"Hi harry potter how are you !!").await?;
        a2b.flush().await?;
        log::info!("Waiting for stream");

        let b2a = socket_b_driver.drive().await?;
        assert_eq!(b2a.id(), a2b.id());
        assert_eq!(*b2a.peer_addr(), socket_a.local_addr()?);

        Ok(())
    }
}
