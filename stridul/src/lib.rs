#![feature(int_roundings)]
#![feature(async_fn_in_trait)]
#![feature(io_error_other)]

#![allow(unused_imports)]
#![allow(dead_code)]

mod one_writer_rwlock;
mod socket;
use bytes::BufMut;
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
    #[error(transparent)]
    Other(#[from] anyhow::Error),
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

    fn serialize(packet: &impl serde::Serialize, into: &mut impl BufMut)
        -> Result<(), StridulError> {
        Ok(bincode::serialize_into(into.writer(), packet)?)
    }
    fn deserialize<D>(bytes: &[u8]) -> Result<D, StridulError>
        where D: for<'a> serde::Deserialize<'a> {
        Ok(bincode::deserialize_from(bytes)?)
    }
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
    use std::collections::VecDeque;
    use std::{collections::HashMap, sync::Mutex};

    use bytes::{BytesMut, BufMut, Bytes};
    use tokio::sync::{futures::Notified, Notify};
    use tokio::io::AsyncReadExt;
    use rand::prelude::*;

    use crate::*;

    #[derive(Default)]
    struct AllSockets {
        sockets: Mutex<HashMap<u32, Arc<LocalSocket>>>,
    }

    struct LocalSocket {
        drop_rate: f64,
        drop_rng: Mutex<SmallRng>,

        all: Arc<AllSockets>,
        new_data: Notify,
        recv_buffer: Mutex<VecDeque<(u32, Bytes)>>,
        addr: u32,
    }

    impl LocalSocket {
        fn new(all: &Arc<AllSockets>, addr: u32, seed: u64, drop_rate: f64) -> Arc<Self> {
            let this = Arc::new(Self {
                drop_rate,
                drop_rng: Mutex::new(SmallRng::seed_from_u64(seed)),
                
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
            if self.drop_rng.lock().unwrap().gen_bool(self.drop_rate) {
                log::debug!("Oups dropped !");
                return Ok(bytes.len());
            }

            let sockets = self.all.sockets.lock().unwrap();
            let t = sockets.get(target).unwrap();
            t.recv_buffer.lock().unwrap().push_front((self.addr, Bytes::from(bytes.to_vec())));
            t.new_data.notify_one();
            Ok(bytes.len())
        }

        async fn recv_from(
            &self, bytes: &mut [u8]
        ) -> Result<(usize, u32), StridulError> {
            let (sender, data) = loop {
                if let Some(x) = self.recv_buffer.lock().unwrap().pop_back()
                { break x; };
                self.new_data.notified().await;
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
        const BASE_RTO: Duration = Duration::from_millis(10);
        const PACKET_MAX_SIZE: u32 = 8;

        fn serialize(packet: &impl serde::Serialize, into: &mut impl BufMut)
            -> Result<(), StridulError> {
            Ok(serde_json::to_writer(into.writer(), packet).map_err(anyhow::Error::from)?)
        }
        fn deserialize<D>(bytes: &[u8]) -> Result<D, StridulError>
            where D: for<'a> serde::Deserialize<'a> {
            Ok(serde_json::from_slice(bytes).map_err(anyhow::Error::from)?)
        }
    }

    #[tokio::test]
    async fn full_a_to_b() -> Result<(), StridulError> {
        let _ = env_logger::builder().is_test(true).try_init();

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, 0, 0.);
        let (socket_a, mut socket_a_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1, 0, 0.);
        let (socket_b, mut socket_b_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_b);

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr()?)
            .await;
        log::info!("Waiting for write");
        let input = b"Hi harry potter how are you !!!";
        a2b.write(input).await?;
        a2b.flush().await?;
        log::info!("Waiting for stream");

        let b2a = tokio::time::timeout(
            Duration::from_millis(1000),
            socket_b_driver.drive(),
        ).await.unwrap()?;
        assert_eq!(b2a.id(), a2b.id());
        assert_eq!(*b2a.peer_addr(), socket_a.local_addr()?);

        tokio::spawn(async move {
            loop {
                socket_b_driver.drive().await.expect("Driving crashed");
            }
        });

        let mut output = BytesMut::zeroed(input.len());
        let _ = tokio::time::timeout(
            Duration::from_millis(1000),
            b2a.reader().read_exact(&mut output)
        ).await.unwrap()?;

        assert_eq!(input.as_slice(), &output[..]);

        Ok(())
    }

    #[tokio::test]
    async fn ab_with_packet_drops() -> Result<(), StridulError> {
        let _ = env_logger::builder().is_test(true).try_init();

        let seed1: u64 = 930485893058898908;
        let seed2: u64 = 4859852851008529200;
        let packet_drop: f64 = 0.25;

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, seed1, packet_drop);
        let (socket_a, mut socket_a_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1, seed2, packet_drop);
        let (socket_b, mut socket_b_driver) =
            StridulSocket::<LocalStrategy>::new(local_socket_b);
        tokio::spawn(async move {
            loop {
                socket_b_driver.drive().await.expect("Driving crashed");
            }
        });

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr()?)
            .await;
        log::info!("Waiting for write");
        let input = b"This message is offfered to you by a very prestigeous company !!!!!";
        a2b.write(input).await?;
        a2b.flush().await?;
        log::info!("Waiting for stream");

        let b2a = socket_b.get_or_create_stream(a2b.id(), socket_a.local_addr()?)
            .await;

        let mut output = BytesMut::zeroed(input.len());
        let _ = tokio::time::timeout(
            Duration::from_millis(2000),
            b2a.reader().read_exact(&mut output)
        ).await.unwrap()?;
        assert_eq!(input.as_slice(), &output[..]);
        Ok(())
    }
}
