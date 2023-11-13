use super::*;

use crate::compiler::{AnyType, TypeVar};
use crate::parser::ast::{ self, Node as AstNode, NodeContainer as AstContainer, AnyNodeKind };
use crate::try_as::{ TryAsRef, TryAsMut };

use std::{borrow::Cow, ops::ControlFlow};
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::atomic::AtomicU64;
use std::any;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeBind {
    Eq {
        to: PrecAnyTy,
    },
}

#[derive(Debug)]
pub struct PrecContext<'a> {
    /// Various equality or else type binds
    type_binds: HashMap<PrecAnyTy, Vec<TypeBind>>,
    /// Fully resolved values of type variables
    type_vars: HashMap<TypeVar, Option<PrecAnyTy>>,

    nodes: HashMap<ast::NodeId, ast::AnyNodeRef<'a>>,
    metas: HashMap<(any::TypeId, ast::NodeId), AnyMeta>,
}

impl<'a> PrecContext<'a> {
    pub fn process_files(files: &'a [&'a ast::File<'a>]) -> Self {
        let mut this = PrecContext {
            type_binds: HashMap::new(),
            type_vars: HashMap::new(),

            nodes: Default::default(),
            metas: Default::default(),
        };

        this._process_files(files);

        this
    }

    fn _process_files(&mut self, files: &'a [&'a ast::File<'a>]) {
        let mut v = PrecVisitor::new(self);
        for f in files.iter() {
            f.walk(&mut v);
        }
    }

    pub fn add_any_node(&mut self, node: ast::AnyNodeRef<'a>) {
        self.nodes.insert(node.any_id(), node.into());
    }

    pub(in crate::compiler) fn add_node<N: ast::Node<'a>>(&mut self, node: &'a N)
        where ast::AnyNodeRef<'a>: From<&'a N>,
    {
        self.add_any_node(node.into());
    }

    pub fn get_any_node(
        &self, node: ast::NodeId
    ) -> Option<ast::AnyNodeRef<'a>> {
        self.nodes.get(&node).copied()
    }

    pub fn get_node<N>(
        &self, node: ast::NodeId
    ) -> Option<&N>
        where ast::AnyNodeRef<'a>: TryAsRef<N> + TryAsMut<N>
    {
        self.nodes.get(&node)
            .and_then(|x| x.try_as_ref())
    }

    pub(in crate::compiler) fn add_meta<M: Meta<'a>>(&mut self, meta: M) -> &mut M
        where AnyMeta: TryAsRef<M> + TryAsMut<M> + From<M>,
    {
        let nid = meta.node_id();
        self.metas.insert(
            (any::TypeId::of::<M::Static>(), nid), meta.into()
        );
        self.get_meta_mut(nid).expect("Insertion cannot fail")
    }

    pub fn get_meta<M: Meta<'a>>(
        &self, node: ast::NodeId
    ) -> Option<&M>
        where AnyMeta: TryAsRef<M> + TryAsMut<M> + From<M>,
    {
        self.metas.get(&(any::TypeId::of::<M::Static>(), node))
            .map(|x| x.try_as_ref().expect("Wrong meta type inserted"))
    }

    pub(in crate::compiler) fn get_meta_mut_or_insert<M, E>(
        &mut self, node: ast::NodeId, val: E
    ) -> &mut M
        where M: Meta<'a>,
              AnyMeta: TryAsRef<M> + TryAsMut<M> + From<M>,
              E: FnOnce(&mut Self) -> M,
    {
        let k = (any::TypeId::of::<M::Static>(), node);
        if !self.metas.contains_key(&k) {
            let v = val(self).into();
            self.metas.insert(k.clone(), v);
        }
        self.metas.get_mut(&k).unwrap().try_as_mut().expect("Wrong meta type inserted")
    }

    pub(in crate::compiler)  fn get_meta_mut<M: Meta<'a>>(
        &mut self, node: ast::NodeId
    ) -> Option<&mut M>
        where AnyMeta: TryAsRef<M> + TryAsMut<M> + From<M>,
    {
        self.metas.get_mut(&(any::TypeId::of::<M::Static>(), node))
            .and_then(|x| x.try_as_mut())
    }

    pub(in crate::compiler) fn get_new_type_var(&mut self) -> TypeVar {
        let var = TypeVar::next_static();
        self.type_vars.insert(var, None);
        var
    }

    pub(in crate::compiler) fn add_type_bind(&mut self, a: PrecAnyTy, b: TypeBind) {
        // FIXPERF: Allocation
        self.type_binds.entry(a)
            .or_insert(vec![])
            .push(b);
    }

    pub(in crate::compiler) fn add_eq_type_bind<A, B>(
        &mut self, a: A, b: B
    )
        where A: Into<PrecAnyTy>,
              B: Into<PrecAnyTy>
    {
        self.add_type_bind(a.into(), TypeBind::Eq { to: b.into() })
    }

    pub(in crate::compiler) fn type_binds(&mut self, of: &PrecAnyTy) -> &[TypeBind] {
        self.type_binds.get(of).map(|x| x.as_slice()).unwrap_or(&[])
    }

    pub(in crate::compiler) fn node_type_var(
        &mut self, node: ast::NodeId
    ) -> TypeVar {
        self.get_meta_mut_or_insert(node, |this| {
            ValueTypeMeta {
                node,
                value_type: this.get_new_type_var(),
            }
        }).value_type
    }

    // /// Quality of life function for binding the type of Node got from the
    // /// [ValueTypeMeta] to the given [TypeVarId]
    // ///
    // /// If node doesn't have a [ValueTypeMeta] associated, one is created
    // /// before creating the type bind
    // pub(in crate::compiler) fn bind_node_type(
    //     &mut self, node: ast::NodeId, to: TypeBind
    // ) {
    //     let tyv = self.get_meta_mut_or_insert(node, |this| {
    //         ValueTypeMeta {
    //             node,
    //             value_type: this.get_new_type_var(),
    //         }
    //     }).value_type;
    //     self.add_type_bind(tyv, to);
    // }

    // /// Quality of life function for binding the type of Node got from the
    // /// [ValueTypeMeta] to the type of another one
    // ///
    // /// If one of the nodes doesn't have a [ValueTypeMeta] associated,
    // /// one is created before creating the type bind
    // pub(in crate::compiler) fn bind_nodes_eq(
    //     &mut self, a: ast::NodeId, b: ast::NodeId
    // ) {
    //     let tya = self.get_meta_mut_or_insert(a, |this| {
    //         ValueTypeMeta {
    //             node: a,
    //             value_type: this.get_new_type_var(),
    //         }
    //     }).value_type;
    //     let tyb = self.get_meta_mut_or_insert(b, |this| {
    //         ValueTypeMeta {
    //             node: b,
    //             value_type: this.get_new_type_var(),
    //         }
    //     }).value_type;
    //     self.add_type_bind(tya, TypeBind::Eq { to: PrecAnyTy::from(tyb) });
    //     self.add_type_bind(tyb, TypeBind::Eq { to: PrecAnyTy::from(tya) });
    // }
}
