use crate::parser::ast;
use crate::try_as::{ TryAsRef, TryAsMut };
use crate::compiler::{prec::PrecAnyTy, TypeVar};

use macros::TryAs;

/// Meta information gathered by Prec about an Ast node
pub trait Meta<'a>: Sized
    where AnyMeta: TryAsRef<Self> + TryAsMut<Self> + From<Self>
{
    type Static: 'static;

    fn node_id(&self) -> ast::NodeId;
}

#[derive(TryAs, Debug)]
pub enum AnyMeta {
    FuncsParamsTypes(FuncsParamsTypesMeta),
    ValueType(ValueTypeMeta),
    Machine(MachineMeta),
    StateMeta(StateMeta),
}

#[derive(Debug)]
pub struct FuncsParamsTypesMeta {
    pub node: ast::NodeId,
    pub params_types: Vec<TypeVar>,
}

impl<'a> Meta<'a> for FuncsParamsTypesMeta {
    type Static = FuncsParamsTypesMeta;

    fn node_id(&self) -> ast::NodeId {
        self.node
    }
}

/// Meta information about the value type of an Ast node
/// this can be the value of an expression or the return value of 
/// a function
#[derive(Debug)]
pub struct ValueTypeMeta {
    pub node: ast::NodeId,
    pub value_type: TypeVar,
}

impl<'a> Meta<'a> for ValueTypeMeta {
    type Static = ValueTypeMeta;

    fn node_id(&self) -> ast::NodeId {
        self.node
    }
}

/// Meta information about a machine
#[derive(Debug)]
pub struct MachineMeta {
    pub node: ast::NodeId,

    pub states: Vec<ast::NodeId>,
    pub initial_state: ast::NodeId,
}

impl<'a> Meta<'a> for MachineMeta {
    type Static = MachineMeta;

    fn node_id(&self) -> ast::NodeId {
        self.node
    }
}

/// Meta information about a machine's state
#[derive(Debug)]
pub struct StateMeta {
    pub node: ast::NodeId,

    pub data: Option<ast::NodeId>,
    pub dyns: Box<[ast::NodeId]>,
    pub transitions: Box<[ast::NodeId]>,
}

impl<'a> Meta<'a> for StateMeta {
    type Static = MachineMeta;

    fn node_id(&self) -> ast::NodeId {
        self.node
    }
}
