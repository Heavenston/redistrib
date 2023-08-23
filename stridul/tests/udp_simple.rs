use std::net::SocketAddr;
use std::time::Duration;
use std::{sync::OnceLock, time::Instant};
use std::io::Write;

use bytes::BytesMut;
use stridul::*;
use tokio::{net::UdpSocket, io::AsyncReadExt, time::timeout};
use rand::prelude::*;

#[derive(Debug, Clone, Copy)]
pub struct MyStridulUDPStrategy;
impl StridulStrategy for MyStridulUDPStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = 1024;
    const BASE_RTO: Duration = Duration::from_millis(100);
    const PACKET_MAX_SIZE: u32 = 512;
    const BUFFER_MAX_SIZE: usize = 1024;
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_simple() -> anyhow::Result<()> {
    pub static START: OnceLock<Instant> = OnceLock::new();

    let _ = env_logger::builder()
        .is_test(true)
        .format(|buf, record| {
            let elapsed = START.get_or_init(|| Instant::now()).elapsed();
            write!(buf,
                "[{}ms {} {}] {}\n",
                elapsed.as_millis(),
                record.module_path().unwrap_or(""),
                record.level(), record.args()
            )
        })
        .filter_level(log::LevelFilter::Trace)
        .try_init();

    let (socket_a, socket_a_driver) = StridulSocket::<MyStridulUDPStrategy>::new(
        UdpSocket::bind("127.0.0.1:0").await?
    );
    log::info!("Connected socket a at addr {:?}", socket_a.local_addr()?);
    socket_a_driver.self_driving();
    let (socket_b, socket_b_driver) = StridulSocket::<MyStridulUDPStrategy>::new(
        UdpSocket::bind("127.0.0.1:0").await?
    );
    log::info!("Connected socket b at addr {:?}", socket_b.local_addr()?);
    socket_b_driver.self_driving();

    let a = {
        let a2b = socket_a.get_or_create_stream(0, socket_b.local_addr()?).await;
        let b2a = socket_b.get_or_create_stream(0, socket_a.local_addr()?).await;

        let mut rng = SmallRng::seed_from_u64(2849032480);

        let a2b_message = (0..128)
            .flat_map(|_| rng.gen::<[u8; 32]>())
            .collect::<BytesMut>();
        log::info!("a2b buffer len: {}", a2b_message.len());

        a2b.write(&a2b_message).await?;
        a2b.flush().await?;

        tokio::spawn(async move {
            let mut a2b_received = BytesMut::zeroed(a2b_message.len());
            timeout(
                Duration::from_millis(2000),
                b2a.reader().read_exact(&mut a2b_received),
            ).await.unwrap()?;
            assert_eq!(&a2b_received[..], &a2b_message[..]);

            Ok::<(), StridulError>(())
        })
    };

    let b = {
        let a2b = socket_a.get_or_create_stream(0, socket_b.local_addr()?).await;
        let b2a = socket_b.get_or_create_stream(0, socket_a.local_addr()?).await;

        let mut rng = SmallRng::seed_from_u64(2849032480);

        let b2a_message = (0..128)
            .flat_map(|_| rng.gen::<[u8; 32]>())
            .collect::<BytesMut>();
        log::info!("b2a Buffer len: {}", b2a_message.len());

        b2a.write(&b2a_message).await?;
        b2a.flush().await?;

        tokio::spawn(async move {
            let mut b2a_received = BytesMut::zeroed(b2a_message.len());
            timeout(
                Duration::from_millis(2000),
                a2b.reader().read_exact(&mut b2a_received),
            ).await.unwrap()?;
            assert_eq!(&b2a_received[..], &b2a_message[..]);

            Ok::<(), StridulError>(())
        })
    };

    a.await.unwrap()?;
    b.await.unwrap()?;

    Ok(())
}
