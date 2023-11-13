use super::*;

use std::{borrow::Cow, ops::ControlFlow};
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::atomic::AtomicU64;
use std::any;

use crate::compiler::{UnitType, BoolType, StringType, IntType, FunctionType};
use crate::parser::ast::{ self, Node as AstNode, NodeContainer as AstContainer, AnyNodeKind };
use crate::try_as::{ TryAsRef, TryAsMut };

// The 's lifetime is required because Prec is invariant on 'a so can't
// be reffered with the same lifetime as 's
pub struct PrecVisitor<'s, 'a> {
    scope_stack: Vec<Scope<'a>>,
    ctx: &'s mut PrecContext<'a>,
}

impl<'s, 'a> PrecVisitor<'s, 'a> {
    pub fn new(ctx: &'s mut PrecContext<'a>) -> Self {
        Self {
            scope_stack: vec![],
            ctx,
        }
    }

    fn push_new_scope(&mut self) {
        self.scope_stack.push(Scope {
            symbols: HashMap::new(),
        });
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn get_symbol<B>(
        &self, name: &'a str
    ) -> Option<&B>
        where B: SymbolBind<'a>,
              AnySymbolBind<'a>: TryAsRef<B>
    {
        for s in self.scope_stack.iter().rev() {
            let Some(s) = s.symbols.get(&(B::NAMESPACE, name))
            else { continue; };
            return s.try_as_ref();
        }

        None
    }

    /// Define a symbol on the top stack
    ///
    /// Panics if the scope stack is empty
    fn define_symbol<B>(
        &mut self, name: &'a str, val: B
    )
        where B: SymbolBind<'a>,
              AnySymbolBind<'a>: From<B>,
    {
        let scope = 
            self.scope_stack.last_mut().expect("Cannot define if no stack");

        let entry = scope.symbols.entry((B::NAMESPACE, name));

        let any_val = AnySymbolBind::from(val);

        use std::collections::hash_map::Entry;
        match entry {
            Entry::Occupied(o) if o.get() != &any_val => todo!("Do something"),
            Entry::Vacant(v) => {
                v.insert(any_val);
            },
            _ => (),
        }
    }
}

impl<'s, 'a> ast::AstVisitor<'a> for PrecVisitor<'s, 'a> {
    fn visit_any(
        &mut self, node_ref: ast::AnyNodeRef<'a>
    ) {
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
                let Some(referee) = self.get_symbol::<ValueBind>(
                    &i.op.content
                ) else { todo!("Uknown symbol error not handled") };

                let fun = match referee {
                    ValueBind::Intrinsic { ty } => ty.clone(),
                    ValueBind::LetBinding { node } => val_var!(node).into(),
                };

                // i :: Left -> (Right -> Result)
                bind_eq!(fun, FunctionType {
                    argument: Box::new(val_var!(i.left).into()),
                    result: Box::new(FunctionType {
                        argument: Box::new(val_var!(i.right).into()),
                        result: Box::new(val_var!(i).into()),
                    }.into()),
                });
            },

            Anr::MachineTypeExpression(machine) => {
                self.push_new_scope();

                let mut initial_state = None;
                // FIXPERF: Allocation
                let mut states = vec![];

                for item in machine.items.iter() {
                    match item {
                        ast::MachineItem::Let(l) => {
                            self.define_symbol(
                                &l.name.content,
                                ValueBind::LetBinding { node: l }
                            );
                        },
                        ast::MachineItem::State(d) => {
                            self.define_symbol(
                                &d.iden.content,
                                StateBind { node: d }
                            );
                            if d.initial_token.is_some() {
                                if initial_state.is_some()
                                { todo!("Multible initial states error not handled") }
                                initial_state = Some(d.id);
                            }
                            states.push(d.id);
                        },
                    }
                }

                let Some(initial_state) = initial_state
                    else { todo!("No initial state error not handled") };

                self.ctx.add_meta(MachineMeta {
                    node: machine.id,
                    states,
                    initial_state,
                });
            },
            Anr::StateDeclaration(s) => {
                if !self.get_symbol::<StateBind>(&s.iden.content)
                    .is_some_and(|id| id.node.id == s.id)
                {
                    todo!("State declaration shoud not be there");
                }

                // FIXPERF: Allocation
                let mut data = None;
                let mut transitions = vec![];
                let mut dyns = vec![];

                for i in s.items.iter() {
                    use ast::StateItem as It;
                    match i {
                        It::Let(i) => {
                            self.define_symbol(
                                &i.name.content,
                                ValueBind::LetBinding { node: i },
                            );
                        },
                        It::Transition(i) => {
                            self.define_symbol(
                                &i.name_id.content,
                                TransitionBind { node: i },
                            );
                            transitions.push(i.id);
                        },
                        It::Dyn(i) => {
                            dyns.push(i.id);
                        },
                        It::Data(i) => {
                            if data.is_some() {
                                todo!("Multible data items in state error not handled");
                            }
                            data = Some(i.id);
                        },
                    }
                }

                self.ctx.add_meta(StateMeta {
                    node: s.id,
                    data,
                    dyns: dyns.into_boxed_slice(),
                    transitions: transitions.into_boxed_slice(),
                });
            },
            Anr::LetBinding(l) => {
                self.define_symbol(
                    &l.name.content,
                    ValueBind::LetBinding { node: l }
                );
            },
            Anr::StateTransition(t) => {
                self.define_symbol(
                    &t.name_id.content,
                    TransitionBind { node: t },
                );
            },
            Anr::Dyn(_) => todo!(),
            Anr::Data(_) => todo!(),
            Anr::DataItem(_) => todo!(),
            Anr::ParentisedExpr(e) => {
                bind_eq!(val_var!(e), val_var!(e.expr));
            },
            Anr::SemiedExpr(e) => {
                bind_eq!(val_var!(e.expr), UnitType);
            },

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
            },
            Anr::IfExpr(iff) => {
                bind_eq!(val_var!(iff), val_var!(iff.then));
                bind_eq!(val_var!(iff.cond), BoolType);
                if let Some((_, else_)) = &iff.else_ {
                    bind_eq!(val_var!(iff.then), val_var!(else_));
                }
            },
            Anr::WhileExpr(whi) => {
                bind_eq!(val_var!(whi), UnitType);
                bind_eq!(val_var!(whi.do_), UnitType);
                bind_eq!(val_var!(whi.cond), BoolType);
            },
            Anr::DecimalLiteral(l) => { bind_eq!(val_var!(l), IntType); },
            Anr::StringLiteral(l) => { bind_eq!(val_var!(l), StringType); },
            Anr::TrueLiteral(l) => { bind_eq!(val_var!(l), BoolType); },
            Anr::FalseLiteral(l) => { bind_eq!(val_var!(l), BoolType); },
            Anr::FunctionApplicationExpr(f) => {
                bind_eq!(val_var!(f.callee), FunctionType {
                    argument: Box::new(val_var!(f.argument).into()),
                    result: Box::new(val_var!(f).into()),
                });
            },
            Anr::IdenExpr(i) => {
                let Some(referee) = self.get_symbol::<ValueBind>(
                    &i.tok.content
                ) else { todo!("Uknown symbol error not handled") };

                let ty = match referee {
                    ValueBind::Intrinsic { ty } => ty.clone(),
                    ValueBind::LetBinding { node } => val_var!(node).into(),
                };

                bind_eq!(val_var!(i), ty);
            },

            // File is only just visited further
            Anr::File(_) => (),
        }

        // Must stay the only logic here as some match branch may return to
        // make there own walk
        node_ref.any_walk(self);
    }
}
