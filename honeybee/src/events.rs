use std::{any::{TypeId, Any}, sync::Arc};

use bytes::Bytes;
use cobweb::events::Event;
use static_assertions as ca;

use crate::Address;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EPacketSend {
    pub destination: Address,
    pub discriminator: u16,
    pub content: Bytes,
}
ca::assert_impl_all!(EPacketSend: Send, Sync);

impl Event for EPacketSend {
    fn event_type_id(&self) -> TypeId { TypeId::of::<Self>() }
    fn to_any(self: Arc<Self>) -> Arc<dyn Send + Sync + Any> { self as Arc<_> }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EPacketRecv {
    pub source: Address,
    pub discriminator: u16,
    pub content: Bytes,
}
ca::assert_impl_all!(EPacketRecv: Send, Sync);

impl Event for EPacketRecv {
    fn event_type_id(&self) -> TypeId { TypeId::of::<Self>() }
    fn to_any(self: Arc<Self>) -> Arc<dyn Send + Sync + Any> { self as Arc<_> }
}
