use std::{sync::Arc, borrow::Cow};

use static_assertions as ca;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    inner: Cow<'static, str>,
}

impl ModuleId {
    pub const fn from_str(name: &'static str) -> Self {
        Self {
            inner: Cow::Borrowed(name)
        }
    }

    pub const fn from_string(name: String) -> Self {
        Self {
            inner: Cow::Owned(name)
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
