use std::sync::Arc;

use anyhow::bail;

use crate::module::{SingletonModule, ModuleId};

pub struct HoneyBee {
    
}

#[async_trait::async_trait]
impl SingletonModule for HoneyBee {
    const ID: ModuleId = ModuleId::from_str("honeybee");

    fn display_name(&self) -> Arc<str> {
        Arc::from("HoneyBee")
    }

    async fn drive(&mut self) -> anyhow::Result<!> {
        bail!("crash")
    }
}
