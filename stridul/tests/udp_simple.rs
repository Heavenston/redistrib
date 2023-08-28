#![feature(int_roundings)]

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use std::{sync::OnceLock, time::Instant};
use std::io::Write;

use bytes::{BytesMut, BufMut};
use env_logger::WriteStyle;
use env_logger::fmt::Color;
use tokio::task::JoinSet;
use tokio::{net::UdpSocket, time::timeout};
use rand::prelude::*;

#[derive(Debug, Clone, Copy)]
pub struct MyUDPStrategy<const BUFFER_SIZE: usize = 4096>;
impl<const BUFFER_SIZE: usize> stridul::Strategy for MyUDPStrategy<BUFFER_SIZE> {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = 1024;
    const BASE_RTO: Duration = Duration::from_millis(100);
    const PACKET_MAX_SIZE: u32 = 512;
    const BUFFER_MAX_SIZE: usize = BUFFER_SIZE;
}

#[derive(Debug, Clone, Copy)]
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

async fn run<Strat: stridul::Strategy<Socket = UdpSocket, PeersAddr = SocketAddr>>(
    run: Run
) -> Result<(), anyhow::Error> {
    pub static START: OnceLock<Instant> = OnceLock::new();

    let _ = env_logger::builder()
        .is_test(option_env!("NO_LOG_TEST").is_none())
        .filter_level(log::LevelFilter::Trace)
        .write_style(WriteStyle::Always)
        .format(|buf, record| {
            let mut style = buf.style();
            use log::Level::*;
            match record.level() {
                Error => style.set_color(Color::Red),
                Warn => style.set_color(Color::Red).set_dimmed(true),
                Info => style.set_color(Color::Blue),
                Debug => style.set_color(Color::White),
                Trace => style.set_color(Color::Black).set_intense(true),
            };
            let elapsed = START.get_or_init(|| Instant::now()).elapsed();

            write!(buf,
                "{}{}{} {}\n",
                style.value(format!("[{}ms {} ",
                    elapsed.as_millis(),
                    record.module_path().unwrap_or(""),
                )),
                style.clone().set_bold(true).value(record.level()),
                style.value("]"),

                record.args()
            )
        })
        .try_init();

    let mut i = 0;

    while option_env!("TEST_FOREVER").is_some() || i == 0 {
        i += 1;
        if option_env!("TEST_FOREVER").is_some() {
            for _ in 0..20 {
                println!("* {i}");
            }
        }

        let mut drivers_joinset = JoinSet::new();

        let mut sockets = Vec::new();
        for i in 0..run.sockets_count {
            let (socket, mut socket_driver) = stridul::Socket::<Strat>::new(
                UdpSocket::bind("127.0.0.1:0").await?
            );
            log::info!("Socket {i} has addr {}", socket.local_addr()?);
            drivers_joinset.spawn(async move {
                loop {
                    socket_driver.drive().await.unwrap();
                }
            });
            sockets.push(socket);
        }
        let sockets = sockets;

        let mut rng = SmallRng::seed_from_u64(2849032480);
        for (ssi, ss) in run.sends.iter().cloned().enumerate() {
            log::info!("Running sequence {ssi}");
            let mut join_set = JoinSet::new();
            for s in ss.iter().copied() {
                let from = Arc::clone(&sockets[s.from]);
                let to = Arc::clone(&sockets[s.to]);

                let from_to = from.get_or_create_stream(s.stream_id, to.local_addr()?).await;
                let to_from = to.get_or_create_stream(s.stream_id, from.local_addr()?).await;

                let sent = (0..s.size.div_ceil(32))
                    .flat_map(|_| rng.gen::<[u8; 32]>())
                    .collect::<BytesMut>();
                log::info!("Sending {} bytes from {} to {}", sent.len(), s.from, s.to);

                log::info!("Writing...");
                from_to.write(&sent).await?;
                from_to.flush().await?;
                log::info!("Creating read task");

                join_set.spawn(async move {
                    let mut received = BytesMut::new();
                    while received.len() < sent.len() {
                        let l = received.len();
                        let read = timeout(
                            Duration::from_millis(2000),
                            to_from.read(&mut (&mut received).limit(
                                sent.len().saturating_sub(l)
                            )),
                        ).await?;
                        log::debug!("[{}->{} on {}] Received {read} bytes ({}/{})",
                            s.from, s.to, s.stream_id,
                            received.len(),
                            sent.len()
                        );
                    }
                    assert_eq!(&sent[..], &received[..]);

                    Ok::<(), anyhow::Error>(())
                });
            }
            while let Some(rslt) = join_set.join_next().await {
                rslt??;
            }
        }
    }

    Ok(())
}

#[tokio::test]
pub async fn udp_ab_ba_singlethreaded() -> anyhow::Result<()> {
    run::<MyUDPStrategy>(Run {
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
    run::<MyUDPStrategy>(Run {
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
pub async fn udp_ab_small_buffer_size() -> anyhow::Result<()> {
    run::<MyUDPStrategy<1024>>(Run {
        sockets_count: 2,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 120,
                size: 4096,
            }
        ], vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 210,
                size: 3255,
            }
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_ab_very_small_buffer_size() -> anyhow::Result<()> {
    run::<MyUDPStrategy<512>>(Run {
        sockets_count: 2,
        sends: vec![vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 59,
                size: 4135,
            }
        ], vec![
            RunSending {
                from: 0,
                to: 1,
                stream_id: 95,
                size: 9816,
            }
        ]],
    }).await
}

#[tokio::test(flavor = "multi_thread")]
pub async fn udp_multistream_multithreaded() -> anyhow::Result<()> {
    run::<MyUDPStrategy>(Run {
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
    run::<MyUDPStrategy>(Run {
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
    run::<MyUDPStrategy>(Run {
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
