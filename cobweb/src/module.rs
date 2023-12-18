use std::{sync::Arc, hash};

use static_assertions as ca;

#[derive(Debug, Clone)]
enum ModuleIdInner {
    Arc(Arc<str>),
    StaticRef(&'static str),
}

impl AsRef<str> for ModuleIdInner {
    fn as_ref(&self) -> &str {
        match self {
            ModuleIdInner::Arc(x) => &**x,
            ModuleIdInner::StaticRef(x) => x,
        }
    }
}

impl PartialEq for ModuleIdInner {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl Eq for ModuleIdInner { }

impl hash::Hash for ModuleIdInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    inner: ModuleIdInner,
}

impl ModuleId {
    pub const fn from_str(name: &'static str) -> Self {
        Self {
            inner: ModuleIdInner::StaticRef(name)
        }
    }

    pub fn from_string(name: String) -> Self {
        Self {
            inner: ModuleIdInner::Arc(Arc::from(name.as_str()))
        }
    }

    pub fn from_arc(name: Arc<str>) -> Self {
        Self {
            inner: ModuleIdInner::Arc(name)
        }
    }
}

#[async_trait::async_trait]
pub trait Module: Send {
    /// unique id for this module, must never change througth the life of the
    /// module
    fn id(&self) -> ModuleId;
    /// debug or log friendly name for this module
    fn display_name(&self) -> Arc<str>;

    async fn drive(&mut self) -> anyhow::Result<!>;
}
ca::assert_obj_safe!(Module);

#[async_trait::async_trait]
pub trait SingletonModule: Module {
    const ID: ModuleId;

    /// debug or log friendly name for this module
    fn display_name(&self) -> Arc<str>;

    async fn drive(&mut self) -> anyhow::Result<!>;
}

#[async_trait::async_trait]
impl<T: SingletonModule> Module for T {
    fn id(&self) -> ModuleId { T::ID }
    fn display_name(&self) -> Arc<str> {
        <T as SingletonModule>::display_name(self)
    }

    async fn drive(&mut self) -> anyhow::Result<!> {
        <T as SingletonModule>::drive(self).await
    }
}
