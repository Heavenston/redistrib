#![feature(never_type)]
#![feature(type_changing_struct_update)]
#![feature(associated_type_defaults)]

// pub(crate) mod socket;
pub mod events;
pub use events::*;
pub mod address;
pub use address::*;
use stridul::PacketStream;
use tokio::task::JoinHandle;

use std::{sync::Arc, marker::PhantomData, net::SocketAddr};

use futures::{SinkExt, StreamExt};
use anyhow::anyhow;
use rand::prelude::*;
use cobweb::{module::{Module, ModuleId}, events::GlobalEventHandler};

pub trait Strategy {
    type Stridul: stridul::Strategy;
    /// Intended to be the same as Self::Stridul but guarentee
    /// to implement Into<Address> and TryFrom<Address>
    type PeersAddr: From<<Self::Stridul as stridul::Strategy>::PeersAddr> +
                    Into<<Self::Stridul as stridul::Strategy>::PeersAddr> +
                    Into<Address> + TryFrom<Address>;
}

#[derive(Debug, Clone, Copy)]
pub struct DefaultUDPStrategy;
impl Strategy for DefaultUDPStrategy {
    type Stridul = stridul::DefaultUDPStrategy;
    type PeersAddr = SocketAddr;
}

pub struct HoneyBeeBuilder<STRAT = (), ID = (), NAME = (), DRIVER = (), SOCKET = (), EVENTS = ()> {
    id: ID,
    name: NAME,
    driver: DRIVER,
    socket: SOCKET,
    events: EVENTS,
    strat: PhantomData<*const STRAT>,
}

impl Default for HoneyBeeBuilder {
    fn default() -> Self {
        Self {
            id: (),
            name: (),
            socket: (),
            driver: (),
            events: (),
            strat: PhantomData,
        }
    }
}

impl<STRAT, NAME, DRIVER, SOCKET, EVENTS> HoneyBeeBuilder<STRAT, (), NAME, DRIVER, SOCKET, EVENTS> {
    pub fn with_id(self, new_id: ModuleId) -> HoneyBeeBuilder<STRAT, ModuleId, NAME, DRIVER, SOCKET, EVENTS> {
        HoneyBeeBuilder {
            id: new_id,
            ..self
        }
    }
}
impl<STRAT, ID, DRIVER, SOCKET, EVENTS> HoneyBeeBuilder<STRAT, ID, (), DRIVER, SOCKET, EVENTS> {
    pub fn with_name(self, name: impl Into<Arc<str>>) -> HoneyBeeBuilder<STRAT, ID, Arc<str>, DRIVER, SOCKET, EVENTS> {
        HoneyBeeBuilder {
            name: name.into(),
            ..self
        }
    }
}
impl<STRAT, NAME, ID, DRIVER, SOCKET> HoneyBeeBuilder<STRAT, ID, NAME, DRIVER, SOCKET, ()> {
    pub fn with_event_handler(self, events: GlobalEventHandler) -> HoneyBeeBuilder<STRAT, ID, NAME, DRIVER, SOCKET, GlobalEventHandler> {
        HoneyBeeBuilder {
            events,
            ..self
        }
    }
}
impl<STRAT, ID, NAME, EVENTS> HoneyBeeBuilder<STRAT, ID, NAME, (), (), EVENTS> {
    pub fn with_socket(
        self,
        socket: <STRAT::Stridul as stridul::Strategy>::Socket,
    ) -> HoneyBeeBuilder<
        STRAT,
        ID,
        NAME,
        stridul::SocketDriver<STRAT::Stridul>,
        Arc<stridul::Socket<STRAT::Stridul>>,
        EVENTS,
    >
        where STRAT: Strategy
    {
        let (socket, driver) = stridul::Socket::new(socket);

        HoneyBeeBuilder {
            driver,
            socket,
            ..self
        }
    }
}

impl<Strat: Strategy> HoneyBeeBuilder<Strat, ModuleId, Arc<str>, stridul::SocketDriver<Strat::Stridul>, Arc<stridul::Socket<Strat::Stridul>>, GlobalEventHandler> {
    pub fn build(mut self) -> HoneyBee<Strat> {
        let (events_sender, events_receiver) = flume::unbounded();

        let driver = self.driver;
        let socket_driver_task = tokio::task::spawn(async move {
            driver_task::<Strat>(events_sender, driver).await
        });

        let addr: Address =
            Strat::PeersAddr::from(self.socket.local_addr()).into();
        self.events.subscribe_filter::<EPacketSend, _>(
            move |f| f.0.source == addr
        );
        
        HoneyBee { 
            id: self.id,
            name: self.name,

            event_handler: self.events,
            stream_handlers: vec![],

            socket_driver_task,
            socket_events: events_receiver,
            socket: self.socket,
        }
    }
}

async fn driver_task<Strat: Strategy>(
    events_sender: flume::Sender<stridul::DrivingEvent<Strat::Stridul>>,
    mut docker_driver: stridul::SocketDriver<Strat::Stridul>,
) -> anyhow::Result<()> {
    loop {
        let event = docker_driver.drive().await?;
        events_sender.send_async(event).await?;
    }
}

/// Use [HoneyBeeBuilder] to create
pub struct HoneyBee<Strat: Strategy> {
    id: ModuleId,
    name: Arc<str>,

    event_handler: GlobalEventHandler,
    stream_handlers: Vec<JoinHandle<()>>,

    socket_driver_task: tokio::task::JoinHandle<anyhow::Result<()>>,
    socket_events: flume::Receiver<stridul::DrivingEvent<Strat::Stridul>>,
    socket: Arc<stridul::Socket<Strat::Stridul>>,
}

#[async_trait::async_trait]
impl<Strat: Strategy> Module for HoneyBee<Strat> {
    fn id(&self) -> ModuleId { self.id.clone() }
    fn display_name(&self) -> Arc<str> { self.name.clone() }

    async fn drive(&mut self) -> anyhow::Result<!> {
        // Listen to the various event sources and calls dedicated functions
        loop {
            // 'Garbage collect' stream handlers
            self.stream_handlers = self.stream_handlers.drain(..)
                .filter(|h| !h.is_finished())
                .collect();

            tokio::select! {
                packet_send = self.event_handler.next_event_of::<EPacketSend>() => {
                    self.handle_packet_send(&*packet_send).await?;
                }

                drive_event = self.socket_events.recv_async() => {
                    let drive_event = drive_event?;
                    self.handle_socket_event(drive_event).await?;
                }
            }
        }
    }
}

impl<Strat: Strategy> HoneyBee<Strat> {
    async fn handle_packet_send(
        &mut self, packet_send: &EPacketSend
    ) -> anyhow::Result<()> {
        let src: <Strat::Stridul as stridul::Strategy>::PeersAddr =
            Strat::PeersAddr::try_from(packet_send.0.source.clone())
                .map_err(|_| unreachable!()).unwrap().into();
        let dest: <Strat::Stridul as stridul::Strategy>::PeersAddr =
            Strat::PeersAddr::try_from(packet_send.0.destination.clone())
                .map_err(|_| anyhow!(
                    "the destination address isn't compatible with this honeybee"
                )).unwrap().into();

        assert_eq!(src, self.socket.local_addr());

        let mut stream_id = [0u8; 4];
        stream_id[0..2].copy_from_slice(
            &packet_send.0.discriminator.to_be_bytes()
        );
        stream_id[2..4].copy_from_slice(
            &rand::thread_rng().gen::<[u8; 2]>()
        );
        let stream_id = u32::from_be_bytes(stream_id);

        let stream = self.socket.create_stream(stream_id, dest).await?;
        let mut packet_stream = stridul::PacketStream::create(Arc::clone(&stream));
        packet_stream.send(packet_send.0.content.clone()).await?;
        
        Ok(())
    }

    /// creates a tokio task to receive packets from from the stream
    /// this prevents not doing anything else while waiting for packets to be
    /// sent through the stream
    /// 
    /// FIXME: do a timeout or something as streams are cheap to create
    ///        and can easily be left without ever finishing
    fn handle_packet_stream(
        &mut self, mut ps: PacketStream<Strat::Stridul>
    ) {
        let events = self.event_handler.reconnect();
        let stream = Arc::clone(ps.inner());
        let discriminator = u16::from_be_bytes(
            stream.id().to_be_bytes()[0..2].try_into().unwrap()
        );
        let source: Address = Strat::PeersAddr::from(
            stream.peer_addr().clone()
        ).into();
        let destination: Address = Strat::PeersAddr::from(
            self.socket.local_addr()
        ).into();

        let handler = tokio::spawn(async move {
            while let Some(content) = ps.next().await {
                let content = content.unwrap();
                
                events.emit(EPacketRecv(Packet {
                    source: source.clone(),
                    destination: destination.clone(),

                    unreliable: false,
                    discriminator,

                    content,
                }));
            }
        });

        self.stream_handlers.push(handler);
    }

    async fn handle_socket_event(
        &mut self, event: stridul::DrivingEvent<Strat::Stridul>
    ) -> anyhow::Result<()> {
        use stridul::DrivingEvent as DE;

        match event {
            DE::NewStream { stream } => {
                self.handle_packet_stream(PacketStream::create(stream));
                Ok(())
            },
            DE::Message { data: _, from: _ } => {
                todo!()
            },
        }
       
    }
}

impl<Strat: Strategy> Drop for HoneyBee<Strat> {
    fn drop(&mut self) {
        self.socket_driver_task.abort();
        for j in &self.stream_handlers {
            j.abort();
        }
    }
}
