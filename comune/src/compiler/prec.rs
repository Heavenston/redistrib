pub mod meta;
use macros::{TryAs, Kind};
use meta::*;
pub mod visitor;
use visitor::*;
pub mod context;
use context::*;
pub mod type_inferer;
use type_inferer::*;

use crate::parser::ast::{ self, Node as AstNode, NodeContainer as AstContainer, AnyNodeKind, NodeId };
use crate::try_as::{ TryAsRef, TryAsMut };
use super::{AnyType, TypeCtx, TypeVar};

use std::{borrow::Cow, ops::ControlFlow};
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::atomic::AtomicU64;
use std::any;

///! Preprocessing step on the AST notably figuring out types

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PrecTyCtx;
impl TypeCtx for PrecTyCtx {
    type Ref = Box<AnyType<Self>>;
}

pub type PrecAnyTy = AnyType<PrecTyCtx>;

#[derive(Debug, thiserror::Error)]
pub enum PrecError {
    
}

pub trait SymbolBind<'a>: Sized {
    const NAMESPACE: SymbolNamespace;
}

/// What a symbol can by bound to
#[derive(Kind, TryAs, Debug, Clone, PartialEq, Eq)]
#[kind(SymbolNamespace)]
pub enum AnySymbolBind<'a> {
    Value(ValueBind<'a>),
    State(StateBind<'a>),
    Transition(TransitionBind<'a>),
}

/// Value Variant of a [SymbolBind]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueBind<'a> {
    LetBinding {
        node: &'a ast::LetBinding<'a>,
    },
    Intrinsic {
        ty: AnyType<PrecTyCtx>,
    },
    Param {
        node: &'a ast::LetBinding<'a>,
        index: usize,
        ty: TypeVar,
    },
}

impl<'a> SymbolBind<'a> for ValueBind<'a> {
    const NAMESPACE: SymbolNamespace = SymbolNamespace::Value;
}

/// State Variant of a [SymbolBind]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateBind<'a> {
    node: &'a ast::StateDeclaration<'a>,
}

impl<'a> SymbolBind<'a> for StateBind<'a> {
    const NAMESPACE: SymbolNamespace = SymbolNamespace::State;
}

/// Transition Variant of a [SymbolBind]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransitionBind<'a> {
    node: &'a ast::StateTransition<'a>,
}

impl<'a> SymbolBind<'a> for TransitionBind<'a> {
    const NAMESPACE: SymbolNamespace = SymbolNamespace::Transition;
}

#[derive(Debug, Clone, Default)]
struct Scope<'a> {
    symbols: HashMap<(SymbolNamespace, &'a str), AnySymbolBind<'a>>,
}

/// Shallow walk the ast to find all nodes of the given types
struct ShallowVisit<C> {
    callback: C,
}

impl<'a, C> ShallowVisit<C>
    where C: FnMut(ast::AnyNodeRef<'a>) -> (),
{
    pub fn new(callback: C) -> Self {
        Self { callback }
    }
}

impl<'a, C> ast::AstVisitor<'a> for ShallowVisit<C>
    where C: FnMut(ast::AnyNodeRef<'a>) -> (),
{
    fn visit_any(
        &mut self, node_ref: ast::AnyNodeRef<'a>
    ) {
        use ast::AnyNodeRef as Anr;

        (self.callback)(node_ref);
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::{ast, Parsable, ParseContext}, compiler::prec::{context::PrecContext, meta::ValueTypeMeta}};

    #[test]
    pub fn test() -> Result<(), Box<dyn std::error::Error>> {
        let src = r#"
            let Main a b = a + b;
        "#;
        // let src = r#"
        //     let main a b = a + b;
        // "#;
        let ast = ast::File::parse(&mut ParseContext::from_src("<test>", src))?;
        println!("AST: {ast}");

        let files = [&ast];
        let ctx = PrecContext::process_files(&files[..]);
        println!("{ctx:#?}");

        assert!(false);
        Ok(())
    }
}
