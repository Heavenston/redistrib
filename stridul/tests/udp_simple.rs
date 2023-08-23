#![feature(int_roundings)]

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use std::{sync::OnceLock, time::Instant};
use std::io::Write;

use bytes::BytesMut;
use stridul::*;
use tokio::task::JoinSet;
use tokio::{net::UdpSocket, io::AsyncReadExt, time::timeout};
use rand::prelude::*;

#[derive(Debug, Clone, Copy)]
pub struct MyStridulUDPStrategy<const BUFFER_SIZE: usize = 4096>;
impl<const BUFFER_SIZE: usize> StridulStrategy for MyStridulUDPStrategy<BUFFER_SIZE> {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = 1024;
    const BASE_RTO: Duration = Duration::from_millis(100);
    const PACKET_MAX_SIZE: u32 = 512;
    const BUFFER_MAX_SIZE: usize = BUFFER_SIZE;
}

#[derive(Debug, Clone)]
struct RunSending {
    from: usize,
    to: usize,
    stream_id: u32,

    size: usize,
}
#[derive(Debug, Clone)]
struct Run {
    sockets_count: usize,
    sends: Vec<Vec<RunSending>>,
}

async fn run<Strat: StridulStrategy<Socket = UdpSocket, PeersAddr = SocketAddr>>(
    run: Run
) -> Result<(), anyhow::Error> {
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

    let mut sockets = Vec::new();
    for i in 0..run.sockets_count {
        let (socket, socket_driver) = StridulSocket::<MyStridulUDPStrategy>::new(
            UdpSocket::bind("127.0.0.1:0").await?
        );
        log::info!("Socket {i} has addr {}", socket.local_addr()?);
        socket_driver.self_driving();
        sockets.push(socket);
    }
    let sockets = sockets;

    let mut rng = SmallRng::seed_from_u64(2849032480);
    for ss in run.sends {
        let mut join_set = JoinSet::new();
        for s in ss {
            let from = Arc::clone(&sockets[s.from]);
            let to = Arc::clone(&sockets[s.to]);

            let from_to = from.get_or_create_stream(s.stream_id, to.local_addr()?).await;
            let to_from = to.get_or_create_stream(s.stream_id, from.local_addr()?).await;

            let sent = (0..s.size.div_ceil(32))
                .flat_map(|_| rng.gen::<[u8; 32]>())
                .collect::<BytesMut>();

            from_to.write(&sent).await?;
            from_to.flush().await?;

            join_set.spawn(async move {
                let mut received = BytesMut::zeroed(sent.len());
                timeout(
                    Duration::from_millis(2000),
                    to_from.reader().read_exact(&mut received),
                ).await.unwrap()?;
                assert_eq!(&sent[..], &received[..]);

                Ok::<(), StridulError>(())
            });
        }
        while let Some(rslt) = join_set.join_next().await {
            rslt??;
        }
    }

    Ok(())
}

#[tokio::test]
pub async fn udp_ab_ba_singlethreaded() -> anyhow::Result<()> {
    run::<MyStridulUDPStrategy>(Run {
        sockets_count: 2,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 423890,
                size: 4096,
            },
            RunSending {
                from: 1,
                to: 0,
                stream_id: 423890,
                size: 4096,
            },
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_ab_ba_multithreaded() -> anyhow::Result<()> {
    run::<MyStridulUDPStrategy>(Run {
        sockets_count: 2,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 423890,
                size: 4096,
            },
            RunSending {
                from: 1,
                to: 0,
                stream_id: 423890,
                size: 4096,
            },
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_multistream_multithreaded() -> anyhow::Result<()> {
    run::<MyStridulUDPStrategy>(Run {
        sockets_count: 2,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 1,
                size: 4096,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 2,
                size: 4096,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 3,
                size: 4096,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 4,
                size: 4096,
            },
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_ab_ac_multithreaded() -> anyhow::Result<()> {
    run::<MyStridulUDPStrategy>(Run {
        sockets_count: 3,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 0,
                size: 30,
            },
            RunSending {
                from: 0,
                to: 2,
                stream_id: 0,
                size: 1249,
            },
            RunSending {
                from: 2,
                to: 1,
                stream_id: 0,
                size: 4031,
            },
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_ab_ab_ab_multithreaded() -> anyhow::Result<()> {
    run::<MyStridulUDPStrategy>(Run {
        sockets_count: 3,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 0,
                size: 4096,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 1,
                size: 2048,
            },
        ], vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 0,
                size: 1000,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 1,
                size: 2586,
            },
        ], vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 0,
                size: 3498,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 1,
                size: 1093,
            },
            RunSending {
                from: 0,
                to: 1,
                stream_id: 2,
                size: 4934,
            },
        ]],
    }).await
}
