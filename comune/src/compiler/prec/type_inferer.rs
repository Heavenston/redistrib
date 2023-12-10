use std::collections::HashMap;

use super::PrecAnyTy;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeBind {
    Eq {
        to: PrecAnyTy,
    },
}

#[derive(Debug)]
pub struct TypeInferer {
    binds: HashMap<PrecAnyTy, TypeBind>,
    vars: Vec<Option<PrecAnyTy>>,
}

impl TypeInferer {
    pub fn new() -> Self {
        Self {
            binds: HashMap::new(),
            vars: Vec::new(),
        }
    }
}

