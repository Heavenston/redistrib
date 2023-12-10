pub mod wasm_helpers;
pub mod prec;

use std::ops::Deref;
use std::fmt::Debug;
use std::hash::Hash;

use macros::TryAs;

mod type_name_id {
    use std::sync::atomic::*;

    /// ID for differentiating nominal types
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TypeNameId {
        id: u32,
    }

    impl TypeNameId {
        pub fn next_static() -> Self {
            static COUNTER: AtomicU32 = AtomicU32::new(0);

            Self { id: COUNTER.fetch_add(1, Ordering::Relaxed) }
        }
    }
}
pub use type_name_id::*;

pub trait TypeCtx: Debug + Send + Sync + Copy + Clone {
    type Ref: Clone + Debug + PartialEq + Eq + Hash;
}

pub trait Type<Ctx: TypeCtx> { } 

#[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyType<Ctx: TypeCtx> {
    Unit(UnitType),
    Bool(BoolType),
    Int(IntType),
    Float(FloatType),
    String(StringType),
    Type(TypeType),
    Function(FunctionType<Ctx>),
    Machine(MachineType<Ctx>),
    Var(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitType;
impl<Ctx: TypeCtx> Type<Ctx> for UnitType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolType;
impl<Ctx: TypeCtx> Type<Ctx> for BoolType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntType;
impl<Ctx: TypeCtx> Type<Ctx> for IntType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatType;
impl<Ctx: TypeCtx> Type<Ctx> for FloatType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringType;
impl<Ctx: TypeCtx> Type<Ctx> for StringType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeType {
    pub var: TypeVar,
}
impl<Ctx: TypeCtx> Type<Ctx> for TypeType {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType<Ctx: TypeCtx> {
    pub argument: Ctx::Ref,
    pub result: Ctx::Ref,
}
impl<Ctx: TypeCtx> Type<Ctx> for FunctionType<Ctx> {  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MachineTypeStateDataField<Ctx: TypeCtx> {
    pub name: String,
    pub ty: Ctx::Ref,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MachineTypeState<Ctx: TypeCtx, Name = String> {
    pub name: Name,
    pub data_fields: Box<[MachineTypeStateDataField<Ctx>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MachineType<Ctx: TypeCtx> {
    pub type_id: TypeNameId,
    pub states: Box<[MachineTypeState<Ctx>]>,
    pub initial_state: MachineTypeState<Ctx, Option<String>>,
}
impl<Ctx: TypeCtx> Type<Ctx> for MachineType<Ctx> {  }

mod type_var {
    use super::{ Type, TypeCtx };
    use std::sync::atomic::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TypeVar {
        id: u32,
    }
    impl<Ctx: TypeCtx> Type<Ctx> for TypeVar {  }

    impl TypeVar {
        pub fn from_raw(n: u32) -> Self { TypeVar { id: n } }
        pub fn into_raw(self) -> u32 { self.id }
    }
}
pub use type_var::*;
