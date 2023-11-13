pub mod wasm_helpers;
pub mod prec;

use std::ops::Deref;
use std::fmt::Debug;
use std::hash::Hash;

use macros::TryAs;

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
        pub fn next_static() -> Self {
            static COUNTER: AtomicU32 = AtomicU32::new(0);

            Self { id: COUNTER.fetch_add(1, Ordering::Relaxed) }
        }
    }
}
pub use type_var::*;
