use std::{sync::Arc, borrow::Cow};

use static_assertions as ca;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    inner: Cow<'static, str>,
}

impl ModuleId {
    pub fn from_str(name: &'static str) -> Self {
        Self {
            inner: Cow::from(name)
        }
    }

    pub fn from_string(name: String) -> Self {
        Self {
            inner: Cow::from(name)
        }
    }
}

pub trait Module: Send {
    /// unique id for this module, must never change througth the life of the
    /// module
    fn id(&self) -> ModuleId;
    /// unique id for this module, must never change througth the life of the
    /// module
    fn display_name(&self) -> Arc<str>;

    /// Dependencies array, must never change through the life of the
    /// module
    fn dependencies(&self) -> &[ModuleId];
}

ca::assert_obj_safe!(Module);
