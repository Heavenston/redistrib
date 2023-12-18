#![feature(never_type)]
#![feature(type_changing_struct_update)]
#![feature(associated_type_defaults)]

// pub(crate) mod socket;
pub mod events;
pub use events::*;
pub mod address;
pub use address::*;

use std::{sync::Arc, marker::PhantomData, net::SocketAddr};
use cobweb::{module::{Module, ModuleId}, events::GlobalEventHandler};

pub trait Strategy {
    type Stridul: stridul::Strategy;
    type PeersAddr: From<<Self::Stridul as stridul::Strategy>::PeersAddr> +
                    Into<<Self::Stridul as stridul::Strategy>::PeersAddr> +
                    Into<Address>;
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
    pub fn build(self) -> HoneyBee<Strat> {
        let (events_sender, events_receiver) = flume::unbounded();

        let driver = self.driver;
        let socket_driver_task = tokio::task::spawn(async move {
            driver_task::<Strat>(events_sender, driver).await
        });
        
        HoneyBee { 
            id: self.id,
            name: self.name,

            event_handler: self.events,

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
            tokio::select! {
                packet_send = self.event_handler.next_event_of::<EPacketSend>() => {
                    self.handle_packet_sent(&*packet_send).await?;
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
    async fn handle_packet_sent(
        &mut self, packet_send: &EPacketSend
    ) -> anyhow::Result<()> {
        todo!()
    }

    async fn handle_socket_event(
        &mut self, event: stridul::DrivingEvent<Strat::Stridul>
    ) -> anyhow::Result<()> {
        use stridul::DrivingEvent as DE;

        match event {
            DE::NewStream { stream: _ } => {
                todo!()
            },
            DE::Message { data: _, from: _ } => {
                todo!()
            },
        }
        
        Ok(())
    }
}
