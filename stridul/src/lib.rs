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

pub trait Strategy: Debug + Sized + 'static {
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

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;
    use std::sync::{OnceLock, Arc};
    use std::time::Instant;
    use std::{collections::HashMap, sync::Mutex};
    use std::io::Write;
    use std::future::Future;

    use bytes::{BytesMut, Bytes};
    use tokio::sync::Notify;
    use tokio::io::AsyncReadExt;
    use rand::prelude::*;

    use crate::*;

    async fn timeout<F: Future>(f: F) -> anyhow::Result<F::Output> {
        Ok(tokio::time::timeout(Duration::from_millis(2000), f).await?)
    }

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

    #[async_trait::async_trait]
    impl<const MAX: u32> StrategySocket<LocalStrategy<MAX>> for Arc<LocalSocket> {
        fn local_addr(&self) -> u32 {
            self.addr
        }

        async fn send_to(
            &self, bytes: &[u8], target: &u32
        ) -> Result<usize, Error> {
            log::trace!("Sending {} bytes to {}", bytes.len(), *target);

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
        ) -> Result<(usize, u32), Error> {
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

    #[derive(Debug, Clone, Copy)]
    struct LocalStrategy<const MAX: u32 = 8>;
    impl<const MAX: u32> Strategy for LocalStrategy<MAX> {
        type Socket = Arc<LocalSocket>;
        type PeersAddr = u32;

        const BASE_WINDOW_SIZE: u32 = 32;
        const BASE_RTO: Duration = Duration::from_millis(10);
        const PACKET_MAX_SIZE: u32 = MAX;
        const BUFFER_MAX_SIZE: usize = 256;
    }

    fn setup_logger() {
        pub static START: OnceLock<Instant> = OnceLock::new();

        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(log::LevelFilter::Trace)
            .format(|buf, record| {
                let elapsed = START.get_or_init(|| Instant::now()).elapsed();
                write!(buf,
                    "[{}ms {} {}] {}\n",
                    elapsed.as_millis(),
                    record.module_path().unwrap_or(""),
                    record.level(), record.args()
                )
            })
            .try_init();
    }

    #[tokio::test]
    async fn full_a_to_b() -> Result<(), Error> {
        setup_logger();

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, 0, 0.);
        let (socket_a, mut socket_a_driver) =
            Socket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1, 0, 0.);
        let (socket_b, mut socket_b_driver) =
            Socket::<LocalStrategy>::new(local_socket_b);

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr())
            .await?;
        log::info!("Waiting for write");
        let input = b"Hi harry potter how are you !!!";
        a2b.write(input).await?;
        a2b.flush().await?;
        log::info!("Waiting for stream");

        let b2a = tokio::time::timeout(
            Duration::from_millis(1000),
            socket_b_driver.drive(),
        ).await.unwrap()?.into_new_stream().expect("Expected a new stream");
        assert_eq!(b2a.id(), a2b.id());
        assert_eq!(*b2a.peer_addr(), socket_a.local_addr());

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
    async fn ab_ba_with_packet_drops() -> Result<(), Error> {
        setup_logger();

        let seed1: u64 = 930485893058898908;
        let seed2: u64 = 4859852851008529200;
        let packet_drop: f64 = 0.25;

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, seed1, packet_drop);
        let (socket_a, mut socket_a_driver) =
            Socket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1, seed2, packet_drop);
        let (socket_b, mut socket_b_driver) =
            Socket::<LocalStrategy>::new(local_socket_b);
        tokio::spawn(async move {
            loop {
                socket_b_driver.drive().await.expect("Driving crashed");
            }
        });

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr())
            .await?;
        log::info!("Waiting for write");
        let input = b"This message is offfered to you by a very prestigeous company !!!!!";
        let input2 = b"test";
        a2b.write(input).await?;
        a2b.flush().await?;
        a2b.write(input2).await?;
        a2b.flush().await?;
        log::info!("Waiting for stream");

        let b2a = socket_b.get_or_create_stream(a2b.id(), socket_a.local_addr())
            .await?;

        let mut output = BytesMut::zeroed(input.len());
        let _ = tokio::time::timeout(
            Duration::from_millis(2000),
            b2a.reader().read_exact(&mut output)
        ).await.unwrap()?;
        assert_eq!(input.as_slice(), &output[..]);

        let mut output2 = BytesMut::zeroed(input2.len());
        let _ = tokio::time::timeout(
            Duration::from_millis(2000),
            b2a.reader().read_exact(&mut output2)
        ).await.unwrap()?;
        log::trace!("{:?}", String::from_utf8_lossy(&output2));
        assert_eq!(input2.as_slice(), &output2[..]);

        Ok(())
    }

    #[tokio::test]
    async fn abba_parallel() -> Result<(), Error> {
        setup_logger();

        let seed1: u64 = 38759035;
        let seed2: u64 = 42357890;
        let packet_drop: f64 = 0.25;

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, seed1, packet_drop);
        let (socket_a, mut socket_a_driver) =
            Socket::<LocalStrategy>::new(local_socket_a);
        tokio::spawn(async move {
            loop {
                socket_a_driver.drive().await.expect("Driving crashed");
            }
        });

        let local_socket_b = LocalSocket::new(&all, 1, seed2, packet_drop);
        let (socket_b, mut socket_b_driver) =
            Socket::<LocalStrategy>::new(local_socket_b);
        tokio::spawn(async move {
            loop {
                socket_b_driver.drive().await.expect("Driving crashed");
            }
        });

        let a2b = socket_a.get_or_create_stream(10, socket_b.local_addr())
            .await?;
        let b2a = socket_b.get_or_create_stream(a2b.id(), socket_a.local_addr())
            .await?;

        let b2a_input = b"Hi again you !";
        let a2b_input = b"I think this works very well !";
        b2a.write(b2a_input).await?;
        b2a.flush().await?;
        a2b.write(a2b_input).await?;
        a2b.flush().await?;

        let ta = tokio::spawn(async move {
            let mut b2a_output = BytesMut::zeroed(b2a_input.len());
            let _ = tokio::time::timeout(
                Duration::from_millis(2000),
                a2b.reader().read_exact(&mut b2a_output)
            ).await.unwrap()?;
            assert_eq!(b2a_input.as_slice(), &b2a_output[..]);

            Ok::<(), Error>(())
        });

        let tb = tokio::spawn(async move {
            let mut a2b_output = BytesMut::zeroed(a2b_input.len());
            let _ = tokio::time::timeout(
                Duration::from_millis(2000),
                b2a.reader().read_exact(&mut a2b_output)
            ).await.unwrap()?;
            assert_eq!(a2b_input.as_slice(), &a2b_output[..]);

            Ok::<(), Error>(())
        });

        ta.await.unwrap()?;
        tb.await.unwrap()?;

        Ok(())
    }

    #[tokio::test]
    async fn message_simple() -> anyhow::Result<()> {
        setup_logger();

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, 0, 0.);
        let (socket_a, mut driver_a) =
            Socket::<LocalStrategy<32>>::new(local_socket_a);
        let addr_a = socket_a.local_addr();

        let local_socket_b = LocalSocket::new(&all, 1, 0, 0.);
        let (socket_b, mut driver_b) =
            Socket::<LocalStrategy<32>>::new(local_socket_b);
        let addr_b = socket_b.local_addr();

        async fn send_and_check(
            s: &Arc<Socket<LocalStrategy<32>>>,
            d: &mut SocketDriver<LocalStrategy<32>>,
            to: u32,
            bytes: Bytes,
        ) -> anyhow::Result<()> {
            s.send_message(to, bytes.clone()).await?;

            if let DrivingEvent::Message { data, from }
                = timeout(d.drive()).await??
            {
                assert_eq!(&data[..], &bytes[..]);
                assert_eq!(from, s.local_addr());
            }
            else {
                panic!("Not a message");
            }

            Ok(())
        }

        log::info!("a to b");

        send_and_check(
            &socket_a, &mut driver_b, addr_b, Bytes::from_static(b"Bonjour")
        ).await?;

        log::info!("b to a");

        send_and_check(
            &socket_b, &mut driver_a, addr_a, Bytes::from_static(b"Aurevoir")
        ).await?;

        log::info!("Same time");

        tokio::try_join!(
            send_and_check(
                &socket_a, &mut driver_b, addr_b, Bytes::from_static(b"This is me")
            ),
            send_and_check(
                &socket_b, &mut driver_a, addr_a, Bytes::from_static(b"and me to")
            ),
        )?;

        Ok(())
    }

    #[tokio::test]
    async fn loopbacks() -> anyhow::Result<()> {
        setup_logger();

        let all = Arc::new(AllSockets::default());
        let local_socket_a = LocalSocket::new(&all, 0, 0, 0.);
        let (socket_a, mut driver_a) =
            Socket::<LocalStrategy<128>>::new(local_socket_a);
        let addr_a = socket_a.local_addr();

        log::info!("Sending message");

        let message = Bytes::from_static(&b"This is a test"[..]);
        timeout(socket_a.send_message(
            addr_a,
            message.clone(),
        )).await??;

        log::info!("Receiving message");

        match timeout(driver_a.drive()).await?? {
            DrivingEvent::Message { data, from } => {
                assert_eq!(&data[..], &message[..]);
                assert_eq!(from, addr_a);
            },
            _ => panic!("Wrong event"),
        }

        log::info!("Creating stream");

        tokio::spawn(async move {
            loop {
                driver_a.drive().await.unwrap();
            }
        });

        let stream = timeout(socket_a.get_or_create_stream(0, addr_a)).await??;

        log::info!("Writing message to stream");

        timeout(stream.write(&message)).await??;

        log::info!("Flushing");

        timeout(stream.flush()).await??;

        log::info!("Received message from stream");

        let mut received = BytesMut::zeroed(message.len());
        stream.reader().read_exact(&mut received).await?;
        assert_eq!(&received[..], &message[..]);

        Ok(())
    }
}
