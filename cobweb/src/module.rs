use std::{any::TypeId, sync::Arc};

use static_assertions as ca;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ModuleIdInner {
    TypeId {
        id: TypeId,
    },
    DisriminatedTypeId {
        discriminant: u64,
        id: TypeId,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    inner: ModuleIdInner,
}

impl ModuleId {
    pub fn from_type_id(id: TypeId) -> Self {
        Self {
            inner: ModuleIdInner::TypeId { id }
        }
    }

    pub fn from_type_id_with_discriminent(
        id: TypeId, discriminant: u64
    ) -> Self {
        Self {
            inner: ModuleIdInner::DisriminatedTypeId { discriminant, id }
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
