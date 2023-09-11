use std::{net::SocketAddr, sync::{Arc, Weak}, task, pin::Pin};

use bytes::BytesMut;
use futures::Stream;
use tokio::{net::UdpSocket, sync::broadcast, io::{AsyncRead, ReadBuf}};
use tokio_util::io::poll_read_buf;

use crate::packet::Packet;

#[derive(Debug, Clone)]
struct CobwebStridulStrategy;
impl stridul::Strategy for CobwebStridulStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = stridul::DefaultUDPStrategy::BASE_WINDOW_SIZE;
    const BASE_RTO: std::time::Duration = stridul::DefaultUDPStrategy::BASE_RTO;
    const PACKET_MAX_SIZE: u32 = stridul::DefaultUDPStrategy::PACKET_MAX_SIZE;
    const BUFFER_MAX_SIZE: usize = stridul::DefaultUDPStrategy::BUFFER_MAX_SIZE;
}

type SStream = stridul::Stream<CobwebStridulStrategy>;
type SSocket = stridul::Socket<CobwebStridulStrategy>;
type SDriver = stridul::SocketDriver<CobwebStridulStrategy>;

#[derive(Debug, Clone)]
pub(crate) enum SocketDriveEvent {
    Message {
        from: SocketAddr,
        packet: Arc<Packet>,
    },
    Stream {
        stream: Arc<SStream>,
    },
}

struct SharedSocket {
    event_sender: broadcast::Sender<SocketDriveEvent>,
    ssocket: Arc<SSocket>,
}

pub struct Socket {
    shared: Arc<SharedSocket>,
}

impl Socket {
    /// Creates a new socket from the given udp socket
    pub fn new(udp_socket: UdpSocket) -> Self {
        let (ssocket, driver) = SSocket::new(udp_socket);

        let shared = Arc::new(SharedSocket {
            event_sender: broadcast::channel(1024).0,
            ssocket,
        });

        let sn = Arc::downgrade(&shared);
        tokio::spawn(async move {
            driver_task(sn, driver).await.expect("Driver crashed");
        });

        Self {
            shared,
        }
    }
}

async fn driver_task(
    shared_node: Weak<SharedSocket>,
    mut docker_driver: SDriver,
) -> anyhow::Result<()> {
    loop {
        let Some(sn) = shared_node.upgrade()
            else { break; };

        let event = docker_driver.drive().await?;

        use stridul::DrivingEvent as DE;
        match event {
            DE::NewStream { stream } => {
                let _ = sn.event_sender.send(
                    SocketDriveEvent::Stream { stream }
                );
            },
            DE::Message { data, from } => {
                let packet = match bincode::deserialize(&data) {
                    Ok(p) => Arc::new(p),
                    Err(e) => {
                        log::warn!("Invalid message from {from} > {e}");
                        continue;
                    },
                };

                let _ = sn.event_sender.send(
                    SocketDriveEvent::Message { from, packet }
                );
            },
        }
    }

    Ok(())
}

#[ouroboros::self_referencing]
pub struct PacketStream {
    stream: Arc<SStream>,
    buffer: BytesMut,

    #[borrows(stream)]
    #[covariant]
    read: stridul::StreamReader::<'this, CobwebStridulStrategy>,
}

impl PacketStream {
    pub fn create(stream: Arc<SStream>) -> Self {
        PacketStreamBuilder {
            stream,
            buffer: BytesMut::with_capacity(4),
            read_builder: |stream| stream.reader(),
        }.build()
    }
}

impl Stream for PacketStream {
    type Item = anyhow::Result<Arc<Packet>>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut task::Context<'_>
    ) -> task::Poll<Option<Self::Item>> {
        self.get_mut().with_mut(|mut fields| {
            let reader = fields.read;
            tokio::pin!(reader);
            let buffer = fields.buffer;

            fn finished(buf: &mut BytesMut) -> bool {
                if buf.len() < 4 {
                    return false;
                }

                let packet_size =
                    u32::from_be_bytes((&buf[0..4]).try_into().unwrap())
                        as usize;

                buf.reserve((packet_size + 4 - buf.len()).min(4096));
                buf.len() >= packet_size
            }

            while !finished(buffer) {
                match poll_read_buf(reader, cx, buffer) {
                    task::Poll::Ready(Ok(_)) => (),
                    task::Poll::Ready(Err(e)) => return task::Poll::Ready(
                        Some(Err(e.into()))
                    ),
                    task::Poll::Pending =>
                        return task::Poll::Pending,
                }
            }

            let packet_size =
                u32::from_be_bytes((&buffer[0..4]).try_into().unwrap())
                    as usize;

            let p: Packet = match bincode::deserialize(&buffer[4..packet_size]) {
                Err(e) => return task::Poll::Ready(Some(Err(e.into()))),
                Ok(p) => p,
            };

            task::Poll::Ready(Some(Ok(Arc::new(p))))
        })
    }
}
