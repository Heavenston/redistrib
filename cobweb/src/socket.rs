use std::{net::SocketAddr, sync::{Arc, Weak}, task, pin::Pin};

use bytes::{BytesMut, BufMut};
use futures::{Stream, Sink};
use tokio::{net::UdpSocket, sync::broadcast, io::AsyncWrite};
use tokio_util::io::poll_read_buf;
use rand::prelude::*;

use crate::packet::Packet;

#[derive(Debug, Clone)]
pub struct CobwebStridulStrategy;
impl stridul::Strategy for CobwebStridulStrategy {
    type Socket = UdpSocket;
    type PeersAddr = SocketAddr;

    const BASE_WINDOW_SIZE: u32 = stridul::DefaultUDPStrategy::BASE_WINDOW_SIZE;
    const BASE_RTO: std::time::Duration = stridul::DefaultUDPStrategy::BASE_RTO;
    const PACKET_MAX_SIZE: u32 = stridul::DefaultUDPStrategy::PACKET_MAX_SIZE;
    const BUFFER_MAX_SIZE: usize = stridul::DefaultUDPStrategy::BUFFER_MAX_SIZE;
}

pub type SStream = stridul::Stream<CobwebStridulStrategy>;
pub type SSocket = stridul::Socket<CobwebStridulStrategy>;
pub type SDriver = stridul::SocketDriver<CobwebStridulStrategy>;

#[derive(Debug, Clone)]
pub enum SocketDriveEvent {
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

    pub async fn next_event() {

    }

    pub async fn send_message(
        &self, to: SocketAddr, packet: &Packet,
    ) -> anyhow::Result<()> {
        let mut bytes = BytesMut::new();

        bincode::serialize_into((&mut bytes).writer(), packet)?;

        self.shared.ssocket.send_message(to, bytes.freeze())
            .await?;

        Ok(())
    }

    pub async fn stream_to(
        &self, to: SocketAddr
    ) -> anyhow::Result<PacketStream> {
        let id = rand::thread_rng().gen();

        let stream = self.shared.ssocket.get_or_create_stream(id, to)
            .await?;
        let stream = PacketStream::create(stream);

        Ok(stream)
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

// Put in cobweb as util (With socket ?)
#[ouroboros::self_referencing]
pub struct PacketStream {
    stream: Arc<SStream>,
    buffer: BytesMut,
    send_buffer: BytesMut,

    #[borrows(stream)]
    #[covariant]
    read: stridul::StreamReader::<'this, CobwebStridulStrategy>,
    #[borrows(stream)]
    #[not_covariant]
    write: stridul::StreamWriter::<'this, CobwebStridulStrategy>,
}

impl PacketStream {
    pub fn create(stream: Arc<SStream>) -> Self {
        PacketStreamBuilder {
            stream,
            buffer: BytesMut::with_capacity(4),

            read_builder: |stream| stream.reader(),
            write_builder: |stream| stream.writer(),

            send_buffer: BytesMut::new(),
        }.build()
    }
}

impl Stream for PacketStream {
    type Item = anyhow::Result<Arc<Packet>>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut task::Context<'_>
    ) -> task::Poll<Option<Self::Item>> {
        self.get_mut().with_mut(|fields| {
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
                match poll_read_buf(reader.as_mut(), cx, buffer) {
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

impl Sink<Packet> for PacketStream {
    type Error = anyhow::Error;

    fn poll_ready(
        self: Pin<&mut Self>, _cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        // FIXME: Snould it wait for something ?
        task::Poll::Ready(Ok(()))
    }

    fn start_send(
        self: Pin<&mut Self>, item: Packet
    ) -> Result<(), Self::Error> {
        self.get_mut().with_mut(|fields| {
            let sb = fields.send_buffer;

            let size_start = sb.len();
            sb.extend_from_slice(&[0; 4]);
            let content_start = sb.len();

            bincode::serialize_into(sb.writer(), &item)?;

            let size: u32 = (content_start - sb.len()).try_into().unwrap();
            sb[size_start..size_start + 4].copy_from_slice(
                &size.to_be_bytes()
            );

            Ok(())
        })
    }

    fn poll_flush(
        mut self: Pin<&mut Self>, cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        while self.borrow_send_buffer().len() > 0 {
            let g = self.as_mut().with_mut(|fields| {
                let sb = fields.send_buffer;
                let write = fields.write;
                tokio::pin!(write);

                let written = match write.poll_write(cx, sb)? {
                    task::Poll::Ready(w) => w,
                    task::Poll::Pending => return task::Poll::Pending,
                };

                // Remove the written bytes
                sb.copy_within(written.., 0);
                sb.truncate(sb.len() - written);

                task::Poll::Ready(Ok(()))
            });

            match g {
                task::Poll::Ready(Ok(())) => (),
                x => return x,
            }
        }

        task::Poll::Ready(Ok(()))
    }

    fn poll_close(
        self: Pin<&mut Self>, cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        self.poll_flush(cx)
    }
}
