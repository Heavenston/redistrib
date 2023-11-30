use super::*;

use std::any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::atomic::AtomicU64;
use std::{borrow::Cow, ops::ControlFlow};

use crate::compiler::{
    BoolType, FunctionType, IntType, MachineType, MachineTypeState, MachineTypeStateDataField,
    StringType, TypeNameId, TypeType, UnitType,
};
use crate::parser::ast::{
    self, AnyNodeKind, Node as AstNode, NodeContainer as AstContainer, StateDeclaration, StateItem,
};
use crate::try_as::{TryAsMut, TryAsRef};

use itertools::Itertools;

// The 's lifetime is required because Prec is invariant on 'a so can't
// be reffered with the same lifetime as 's
pub struct PrecVisitor<'s, 'a> {
    scope_stack: Vec<Scope<'a>>,
    ctx: &'s mut PrecContext<'a>,
}

impl<'s, 'a> PrecVisitor<'s, 'a> {
    pub fn new(ctx: &'s mut PrecContext<'a>) -> Self {
        let mut this = Self {
            scope_stack: vec![],
            ctx,
        };
        this.push_new_scope();
        this.define_symbol(
            "+",
            ValueBind::Intrinsic {
                ty: FunctionType {
                    argument: Box::new(IntType.into()),
                    result: Box::new(
                        FunctionType {
                            argument: Box::new(IntType.into()),
                            result: Box::new(IntType.into()),
                        }
                        .into(),
                    ),
                }
                .into(),
            },
        );
        this.define_symbol(
            "printf",
            ValueBind::Intrinsic {
                ty: FunctionType {
                    argument: Box::new(IntType.into()),
                    result: Box::new(UnitType.into()),
                }
                .into(),
            },
        );
        this
    }

    fn push_new_scope(&mut self) {
        self.scope_stack.push(Scope {
            symbols: HashMap::new(),
        });
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn get_symbol<B>(&self, name: &'a str) -> Option<&B>
    where
        B: SymbolBind<'a>,
        AnySymbolBind<'a>: TryAsRef<B>,
    {
        for s in self.scope_stack.iter().rev() {
            let Some(s) = s.symbols.get(&(B::NAMESPACE, name)) else {
                continue;
            };
            return s.try_as_ref();
        }

        None
    }

    /// Define a symbol on the top stack
    ///
    /// Panics if the scope stack is empty
    fn define_symbol<B>(&mut self, name: &'a str, val: B)
    where
        B: SymbolBind<'a>,
        AnySymbolBind<'a>: From<B>,
    {
        let scope = self
            .scope_stack
            .last_mut()
            .expect("Cannot define if no stack");

        let entry = scope.symbols.entry((B::NAMESPACE, name));

        let any_val = AnySymbolBind::from(val);

        use std::collections::hash_map::Entry;
        match entry {
            Entry::Occupied(o) if o.get() != &any_val => todo!("Do something"),
            Entry::Vacant(v) => {
                v.insert(any_val);
            }
            _ => (),
        }
    }
}

impl<'s, 'a> ast::AstVisitor<'a> for PrecVisitor<'s, 'a> {
    fn visit_any(&mut self, node_ref: ast::AnyNodeRef<'a>) {
        self.ctx.add_any_node(node_ref);

        macro_rules! val_var {
            ($v:expr) => {
                self.ctx.node_type_var($v.cont_id())
            };
        }

        macro_rules! bind_eq {
            ($a:expr, $b:expr) => {
                let a = $a;
                let b = $b;
                self.ctx.add_eq_type_bind(a, b)
            };
        }

        use ast::AnyNodeRef as Anr;
        match node_ref {
            Anr::Infix(i) => {
                let Some(referee) = self.get_symbol::<ValueBind>(&i.op.content) else {
                    todo!("Uknown symbol error not handled")
                };

                use ValueBind as VB;
                let fun = match referee {
                    VB::Intrinsic { ty } => ty.clone(),
                    VB::LetBinding { node } => val_var!(node).into(),
                    VB::Param { ty: type_var, .. } => (*type_var).into(),
                };

                // i :: Left -> (Right -> Result)
                bind_eq!(
                    fun,
                    FunctionType {
                        argument: Box::new(val_var!(i.left).into()),
                        result: Box::new(
                            FunctionType {
                                argument: Box::new(val_var!(i.right).into()),
                                result: Box::new(val_var!(i).into()),
                            }
                            .into()
                        ),
                    }
                );
            }

            Anr::MachineTypeExpression(machine) => {
                self.push_new_scope();

                let mut initial_state = None;
                // FIXPERF: Allocation
                let mut meta_states: Vec<ast::NodeId> = vec![];
                let mut ty_states: Vec<MachineTypeState<PrecTyCtx>> = vec![];

                for item in machine.items.iter() {
                    match item {
                        ast::MachineItem::Let(l) => {
                            self.define_symbol(&l.name.content, ValueBind::LetBinding { node: l });
                        }
                        ast::MachineItem::State(d) => {
                            if let Some(iden) = &d.iden {
                                self.define_symbol(&iden.content, StateBind { node: d });
                            }

                            let mut fields = vec![];
                            for i in d.items.iter() {
                                let StateItem::Data(data) = i else { continue };
                                for (f, _) in data.items.items.iter() {
                                    let var = self.ctx.get_new_type_var();
                                    fields.push(MachineTypeStateDataField {
                                        name: f.iden.content.to_string(),
                                        ty: Box::new(var.into()),
                                    });
                                }
                            }

                            if d.initial_token.is_some() {
                                if initial_state.is_some() {
                                    todo!("Multible initial states error not handled")
                                }
                                let state = MachineTypeState {
                                    name: d.iden.as_ref().map(|x| x.to_string()),
                                    data_fields: fields.into_boxed_slice(),
                                };
                                initial_state = Some((state, d.id));
                            } else {
                                let state = MachineTypeState {
                                    name: match &d.iden {
                                        Some(x) => x.content.to_string(),
                                        None => todo!("Missing state name on non initial state error not handled"),
                                    },
                                    data_fields: fields.into_boxed_slice(),
                                };
                                meta_states.push(d.id);
                                ty_states.push(state);
                            }
                        }
                    }
                }

                let Some((ty_initial_state, meta_initial_state)) = initial_state else {
                    todo!("No initial state error not handled")
                };

                bind_eq!(val_var!(machine), MachineType {
                    type_id: TypeNameId::next_static(),
                    states: ty_states.into_boxed_slice(),
                    initial_state: ty_initial_state,
                });
                self.ctx.insert_meta(MachineMeta {
                    node: machine.id,
                    states: meta_states,
                    initial_state: meta_initial_state,
                });
            }
            Anr::StateDeclaration(s) => {
                if let Some(iden) = &s.iden {
                    if !self
                        .get_symbol::<StateBind>(&iden.content)
                        .is_some_and(|id| id.node.id == s.id)
                    {
                        todo!("State declaration shoud not be there");
                    }
                }

                // FIXPERF: Allocation
                let mut data = None;
                let mut transitions = vec![];
                let mut dyns = vec![];

                for i in s.items.iter() {
                    use ast::StateItem as It;
                    match i {
                        It::Let(i) => {
                            self.define_symbol(&i.name.content, ValueBind::LetBinding { node: i });
                        }
                        It::Transition(i) => {
                            self.define_symbol(&i.name_id.content, TransitionBind { node: i });
                            transitions.push(i.id);
                        }
                        It::Dyn(i) => {
                            dyns.push(i.id);
                        }
                        It::Data(i) => {
                            if data.is_some() {
                                todo!("Multible data items in state error not handled");
                            }
                            data = Some(i.id);
                        }
                    }
                }

                self.ctx.insert_meta(StateMeta {
                    node: s.id,
                    data,
                    dyns: dyns.into_boxed_slice(),
                    transitions: transitions.into_boxed_slice(),
                });
            }
            Anr::LetBinding(l) => {
                self.define_symbol(&l.name.content, ValueBind::LetBinding { node: l });

                self.push_new_scope();
                for (index, a) in l.params.iter().enumerate() {
                    let ty = self.ctx.get_new_type_var();
                    self.define_symbol(&a.content, ValueBind::Param { node: l, index, ty });
                }
                node_ref.any_walk(self);
                self.pop_scope();

                return;
            }
            Anr::StateTransition(t) => {
                self.define_symbol(&t.name_id.content, TransitionBind { node: t });
            }
            Anr::Dyn(_) => todo!(),
            Anr::Data(_) => todo!(),
            Anr::DataItem(di) => {
                let var = self.ctx.get_new_type_var();
                bind_eq!(val_var!(di.ty), TypeType { var });
            }
            Anr::ParentisedExpr(e) => {
                bind_eq!(val_var!(e), val_var!(e.expr));
            }
            Anr::SemiedExpr(e) => {
                bind_eq!(val_var!(e.expr), UnitType);
            }

            Anr::BlockExpr(b) => {
                self.push_new_scope();
                for i in b.items.iter() {
                    use ast::BlockItem as BI;
                    match i {
                        BI::Let(_) | BI::Expr(_) => (),
                    }
                }
                node_ref.any_walk(self);
                self.pop_scope();
                return;
            }
            Anr::IfExpr(iff) => {
                bind_eq!(val_var!(iff), val_var!(iff.then));
                bind_eq!(val_var!(iff.cond), BoolType);
                if let Some((_, else_)) = &iff.else_ {
                    bind_eq!(val_var!(iff.then), val_var!(else_));
                }
            }
            Anr::WhileExpr(whi) => {
                bind_eq!(val_var!(whi), UnitType);
                bind_eq!(val_var!(whi.do_), UnitType);
                bind_eq!(val_var!(whi.cond), BoolType);
            }
            Anr::DecimalLiteral(l) => {
                bind_eq!(val_var!(l), IntType);
            }
            Anr::StringLiteral(l) => {
                bind_eq!(val_var!(l), StringType);
            }
            Anr::TrueLiteral(l) => {
                bind_eq!(val_var!(l), BoolType);
            }
            Anr::FalseLiteral(l) => {
                bind_eq!(val_var!(l), BoolType);
            }
            Anr::FunctionApplicationExpr(f) => {
                bind_eq!(
                    val_var!(f.callee),
                    FunctionType {
                        argument: Box::new(val_var!(f.argument).into()),
                        result: Box::new(val_var!(f).into()),
                    }
                );
            }
            Anr::IdenExpr(i) => {
                let Some(referee) = self.get_symbol::<ValueBind>(&i.tok.content) else {
                    todo!("Uknown symbol ({}) error not handled", i.tok.content)
                };

                use ValueBind as VB;
                let ty = match referee {
                    VB::Intrinsic { ty } => ty.clone(),
                    VB::LetBinding { node } => val_var!(node).into(),
                    VB::Param { ty: type_var, .. } => (*type_var).into(),
                };

                bind_eq!(val_var!(i), ty);
            }

            // Nothing to be done in file as it is only a container
            Anr::File(_) => (),
        }

        // Must stay the only logic here as some match branch may return to
        // make their own walk
        node_ref.any_walk(self);
    }
}
