use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use std::any::{ Any, TypeId };
use std::fmt::Debug;

use itertools::Itertools;
use static_assertions as ca;
use tokio::sync::broadcast;

pub trait Event: Any + Debug + Sync + Send + 'static {
    fn event_type_id(&self) -> TypeId;
    fn to_any(self: Arc<Self>) -> Arc<dyn Any>;

    fn obj(self) -> EventObj
        where Self: Sized
    {
        self.into()
    }

    fn typed(self) -> TypedEventObj<Self>
        where Self: Sized
    {
        self.obj().typed()
    }
}
ca::assert_obj_safe!(Event);

#[derive(Debug, Clone)]
pub struct EventObj(Arc<dyn Event>);
ca::assert_impl_all!(EventObj: Send, Sync);

impl EventObj {
    fn to_any(&self) -> Arc<dyn Any> {
        self.0.clone().to_any()
    }

    pub fn typed<E: Event>(&self) -> TypedEventObj<E> {
        TypedEventObj::from_obj(self)
    }

    pub fn try_typed<E: Event>(&self) -> Option<TypedEventObj<E>> {
        TypedEventObj::try_from_obj(self)
    }
}

impl Deref for EventObj {
    type Target = dyn Event;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<E: Event> From<E> for EventObj {
    fn from(value: E) -> Self {
        Self(Arc::new(value) as Arc<dyn Event>)
    }
}

#[derive(Clone)]
pub struct TypedEventObj<E: Event> {
    phantom: PhantomData<*const E>,
    any: Arc<dyn Any>,
}

impl<E: Event> TypedEventObj<E> {
    pub fn from_obj(obj: &EventObj) -> Self {
        assert_eq!(obj.event_type_id(), TypeId::of::<E>());
        Self {
            phantom: Default::default(),
            any: obj.to_any(),
        }
    }

    pub fn try_from_obj(obj: &EventObj) -> Option<Self> {
        if obj.event_type_id() != TypeId::of::<E>() {
            return None;
        }

        Some(Self {
            phantom: Default::default(),
            any: obj.to_any(),
        })
    }
}

impl<E: Event> Deref for TypedEventObj<E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        self.any.downcast_ref().expect("wrong type in typed event")
    }
}

impl<E: Event> Debug for TypedEventObj<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TypedEventObj")
            .field(&**self)
            .finish()
    }
}

impl<E> PartialEq for TypedEventObj<E>
    where E: Event + PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        (&**self) == (&**other)
    }
}
impl<E: Event + Eq> Eq for TypedEventObj<E> { }

#[derive(Debug)]
struct EventInner {
    senders: RwLock<
        HashMap<TypeId, broadcast::Sender<EventObj>>
    >,
}

impl Default for EventInner {
    fn default() -> Self {
        Self {
            senders: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct GlobalEventHandler {
    subs: Vec<(TypeId, broadcast::Receiver<EventObj>)>,
    inner: Arc<EventInner>,
}

impl GlobalEventHandler {
    /// Creates a new [Self] not connected to any other one
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [Self] connected to this one
    pub fn reconnect(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            ..Default::default()
        }
    }

    fn get_sender<E: Event>(
        &self
    ) -> Option<broadcast::Sender<EventObj>> {
        self.get_sender_any(TypeId::of::<E>())
    }

    fn get_sender_any(
        &self, type_id: TypeId
    ) -> Option<broadcast::Sender<EventObj>> {
        self.inner.senders.read().unwrap().get(&type_id).cloned()
    }

    fn get_sender_or_insert<E: Event>(
        &self
    ) -> broadcast::Sender<EventObj> {
        self.get_sender_or_insert_any(TypeId::of::<E>())
    }

    fn get_sender_or_insert_any(
        &self, type_id: TypeId
    ) -> broadcast::Sender<EventObj> {
        match self.get_sender_any(type_id) {
            Some(s) => s,
            None => {
                // a race condition here is between reading the entry
                // and getting the write access a thread could have also
                // created the entry, so we need to handle this case here
                self.inner.senders.write().unwrap().entry(type_id)
                    .or_insert(broadcast::channel(2048).0)
                    .clone()
            }
        }
    }

    pub fn subscribe<E: Event>(&mut self) -> bool {
        self.subscribe_any(TypeId::of::<E>())
    }

    pub fn subscribe_any(&mut self, type_id: TypeId) -> bool {
        if self.subs.iter().any(|x| x.0 == type_id) {
            return false;
        }

        let sender = self.get_sender_or_insert_any(type_id);
        let recv = sender.subscribe();

        self.subs.push((type_id, recv));

        true
    }

    pub async fn next_event(&mut self) -> EventObj {
        let waits = self.subs.iter_mut()
            .map(|x| Box::pin(x.1.recv()))
            .collect_vec();
        let (res, _, _) = futures::future::select_all(waits).await;
        res.expect("broadcast recv error")
    }

    pub async fn next_event_of<E: Event>(
        &mut self
    ) -> TypedEventObj<E> {
        self.next_event_of_any(TypeId::of::<E>()).await.typed()
    }

    pub async fn next_event_of_any(
        &mut self, type_id: TypeId
    ) -> EventObj {
        let (.., recv) = self.subs.iter_mut().find(|x| x.0 == type_id)
            .expect("not subscribed to this event");
        recv.recv().await.expect("broadcast recv error")
    }

    pub fn try_next_event(&mut self) -> Option<EventObj> {
        self.subs.iter_mut()
            .find_map(|(_, recv)| match recv.try_recv() {
                Ok(x) => Some(x),
                Err(broadcast::error::TryRecvError::Empty) => None,
                x => { x.expect("broadcast recv error"); None },
            })
    }

    pub fn try_next_event_of<E: Event>(
        &mut self
    ) -> Option<TypedEventObj<E>> {
        let obj = self.try_next_event_of_any(TypeId::of::<E>());
        obj.as_ref().map(EventObj::typed)
    }

    pub fn try_next_event_of_any(
        &mut self, type_id: TypeId
    ) -> Option<EventObj> {
        let (.., recv) = self.subs.iter_mut().find(|x| x.0 == type_id)
            .expect("not subscribed to this event");
        match recv.try_recv() {
            Ok(x) => Some(x),
            Err(broadcast::error::TryRecvError::Empty) => None,
            x => { x.expect("broadcast recv error"); None },
        }
    }

    pub fn emit<E: Into<EventObj>>(&self, obj: E) {
        let obj = obj.into();
        // no sender = no reveiver = no need to send anything
        let Some(sender) = self.get_sender_any(obj.event_type_id())
            else { return; };
        match sender.send(obj) {
            Ok(_) => (),
            // No receiver is not an error here
            Err(broadcast::error::SendError(_)) => (),
        }
    }
}
