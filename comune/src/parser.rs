
pub mod ast {
    use std::marker::PhantomData;
    use std::fmt::{ Debug, Display };
    use std::ops::ControlFlow;
    use std::sync::atomic::AtomicU64;

    use crate::lexer::*;

    use derive_more::*;

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NodeId {
        id: u64,
    }

    impl NodeId {
        pub(super) fn next() -> Self {
            static NODE_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

            Self {
                id: NODE_ID_COUNTER
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            }
        }
    }

    impl Debug for NodeId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "NodeId({})", self.id)
        }
    }

    /// Wether the visit is done before or after children
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum VisitOrder {
        /// Before any child is visited
        Prefix,
        /// After all children are visited
        Suffix,
    }
    use VisitOrder::*;

    /// Ast node but with no Id (only visit its children)
    pub trait Visitable<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()>;
    }

    /// Trait of all nodes in the AST
    pub trait Node {
        /// A unique id for the node
        fn id(&self) -> NodeId;
    }

    pub trait AstVisitor<'a> {
        fn visit_file(
            &mut self, _node: &'a File<'a>,
            _order: VisitOrder
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }

        fn visit_infix_op<Left, Op: KnownToken<'a>, Right>(
            &mut self, _node: &'a InfixOp<'a, Left, Op, Right>,
            _order: VisitOrder
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_function_application(
            &mut self, _node: &'a FunctionApplicationExpr<'a>,
            _order: VisitOrder
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_parentised(
            &mut self,
            _node: &'a ParentisedExpr<'a>,
            _order: VisitOrder
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_block(
            &mut self,
            _node: &'a BlockExpr<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_id(
            &mut self,
            _node: &'a IdenToken<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_if(
            &mut self,
            _node: &'a IfExpr<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_while(
            &mut self,
            _node: &'a WhileExpr<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_semied_expr(
            &mut self,
            _node: &'a SemiedExpr<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }

        fn visit_decimal_literal(
            &mut self,
            _node: &'a DecimalLiteral<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_string_literal(
            &mut self,
            _node: &'a StringLiteral<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_false_literal(
            &mut self,
            _node: &'a FalseLiteral<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_true_literal(
            &mut self,
            _node: &'a TrueLiteral<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_iden_expr(
            &mut self,
            _node: &'a IdenExpr<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }

        fn visit_let(
            &mut self,
            _node: &'a LetDeclaration<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_machine(
            &mut self,
            _node: &'a MachineDeclaration<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_state(
            &mut self,
            _node: &'a StateDeclaration<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_state_transition(
            &mut self,
            _node: &'a StateTransition<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_let_decl(
            &mut self,
            _node: &'a LetDeclaration<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_dyn(
            &mut self,
            _node: &'a Dyn<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_data(
            &mut self,
            _node: &'a Data<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_data_stmt(
            &mut self,
            _node: &'a DataStmt<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_on(
            &mut self,
            _node: &'a On<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
        fn visit_parameter(
            &mut self,
            _node: &'a Parameter<'a>,
            _order: VisitOrder,
        ) -> ControlFlow<()> { ControlFlow::Continue(()) }
    }

    /// Generic Container of statments supporting both surrounding every
    /// statements or just a semi, making every following statements as being
    /// part of the container, until the End token is reached.
    #[derive(Debug)]
    pub enum StmtContainer<'a, End: KnownToken<'a>, T> {
        Never {
            never: !,
            phantom: PhantomData<*const End>,
        },
        UseSemi {
            semi: SemiColonToken<'a>,
            stmts: Box<[T]>,
        },
        UseBrackets {
            open: CurlyOpenToken<'a>,
            stmts: Box<[T]>,
            close: CurlyCloseToken<'a>,
        },
    }

    impl<'a, End, T> Visitable<'a> for StmtContainer<'a, End, T>
        where End: KnownToken<'a>,
              T: Visitable<'a>,
    {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::UseSemi { semi: _, stmts } => {
                    for t in stmts.iter() {
                        t.visit(v)?;
                    }
                },
                Self::UseBrackets { open: _, stmts, close: _ } => {
                    for t in stmts.iter() {
                        t.visit(v)?;
                    }
                },
                Self::Never { .. } => unreachable!(),
            }

            ControlFlow::Continue(())
        }
    }

    impl<'a, End, T> Display for StmtContainer<'a, End, T>
        where End: KnownToken<'a> + Display,
              T: Display
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::UseSemi { semi, stmts } => {
                    write!(f, "{semi}")?;
                    for stmt in stmts.iter() {
                        write!(f, " {stmt}")?;
                    }
                },
                Self::UseBrackets { open, stmts, close } => {
                    write!(f, "{open}")?;
                    for stmt in stmts.iter() {
                        write!(f, " {stmt}")?;
                    }
                    write!(f, " {close}")?;
                },
                Self::Never { .. } => unreachable!(),
            }

            Ok(())
        }
    }

    /// List of nodes separated by a token, usually a comma (ex. arguments)
    /// with optional trailing separator
    #[derive(Debug)]
    pub struct SeparatedList<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T> {
        pub phantom: PhantomData<*const &'a End>,

        pub stmts: Box<[(T, Sep)]>,
        /// Last statement is the only one with optional separator
        pub last: Option<(Box<T>, Option<Sep>)>,
    }

    impl<'a, End, Sep, T> Visitable<'a> for SeparatedList<'a, End, Sep, T>
        where End: KnownToken<'a> + Display,
              Sep: KnownToken<'a> + Display,
              T: Visitable<'a>
    {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            for (s, _) in self.stmts.iter() {
                s.visit(v)?;
            }
            ControlFlow::Continue(())
        }
    }

    impl<'a, End, Sep, T> Display for SeparatedList<'a, End, Sep, T>
        where End: KnownToken<'a> + Display,
              Sep: KnownToken<'a> + Display,
              T: Display
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for (t, sep) in self.stmts.iter() {
                write!(f, "{t}{sep}")?;
            }
            if let Some((t, sep)) = &self.last {
                write!(f, "{t}")?;
                if let Some(sep) = sep {
                    write!(f, "{sep}")?;
                }
            }

            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct LetDeclaration<'a> {
        pub id: NodeId,

        pub let_: LetToken<'a>,
        pub mut_: Option<MutToken<'a>>,
        pub name: IdenToken<'a>,
        pub params: Box<[IdenToken<'a>]>,
        pub eq: EqualToken<'a>,
        pub expr: Expr<'a>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node for LetDeclaration<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for LetDeclaration<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_let(self, Prefix)?;

            self.expr.visit(v)?;

            v.visit_let(self, Suffix)?;
            ControlFlow::Continue(())
        }
    }

    impl<'a> Display for LetDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.let_)?;
            if let Some(mut_) = &self.mut_ {
                write!(f, " {}", mut_)?;
            }
            write!(f, " {}", self.name)?;
            for params in self.params.iter() {
                write!(f, " {}", params)?;
            }
            write!(f, " {}", self.eq)?;
            write!(f, " {}", self.expr)?;
            write!(f, "{}", self.semi)?;

            Ok(())
        }
    }

    /// A Machine declaration
    /// Ex:
    /// ```comune
    /// machine test {
    ///     initial state init {
    ///        [...]
    ///     }
    ///
    ///     state second {
    ///        [...]
    ///     }
    /// }
    /// ```
    #[derive(Debug)]
    pub struct MachineDeclaration<'a> {
        pub id: NodeId,

        pub machine_token: MachineToken<'a>,
        pub iden: IdenToken<'a>,
        pub stmts: StmtContainer<'a, MachineToken<'a>, MachineStmt<'a>>,
    }

    impl<'a> Node for MachineDeclaration<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for MachineDeclaration<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_machine(self, Prefix)?;

            self.stmts.visit(v)?;

            v.visit_machine(self, Suffix)?;

            ControlFlow::Continue(())
        }
    }

    impl<'a> Display for MachineDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ", self.machine_token)?;
            write!(f, "{} ", self.iden)?;
            write!(f, "{}", self.stmts)?;

            Ok(())
        }
    }

    /// Statements that go into a Machine
    #[derive(Debug, From)]
    pub enum MachineStmt<'a> {
        Declaration(Declaration<'a>),
        State(StateDeclaration<'a>),
    }

    impl<'a> Visitable<'a> for MachineStmt<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::Declaration(e) => e.visit(v),
                Self::State(e) => e.visit(v),
            }
        }
    }

    impl<'a> Display for MachineStmt<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                MachineStmt::Declaration(s) => write!(f, "{s}")?,
                MachineStmt::State(s) => write!(f, "{s}")?,
            }

            Ok(())
        }
    }

    /// A State declaration
    #[derive(Debug)]
    pub struct StateDeclaration<'a> {
        pub id: NodeId,

        pub initial_token: Option<InitialToken<'a>>,
        pub state_token: StateToken<'a>,
        pub iden: IdenToken<'a>,
        pub stmts: StmtContainer<'a, StateToken<'a>, StateStmt<'a>>,
    }

    impl<'a> Node for StateDeclaration<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for StateDeclaration<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_state(self, Prefix)?;

            self.stmts.visit(v)?;

            v.visit_state(self, Suffix)
        }
    }

    impl<'a> Display for StateDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(initial) = &self.initial_token {
                write!(f, "{} ", initial)?;
            }
            write!(f, "{} ", self.state_token)?;
            write!(f, "{} ", self.iden)?;
            write!(f, "{}", self.stmts)?;

            Ok(())
        }
    }

    /// Statements that go into a State
    #[derive(Debug, From)]
    pub enum StateStmt<'a> {
        Declaration(Declaration<'a>),
        Transition(StateTransition<'a>),
        Dyn(Dyn<'a>),
        Data(Data<'a>),
        On(On<'a>),
    }

    impl<'a> Visitable<'a> for StateStmt<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::Declaration(e) => e.visit(v),
                Self::Transition(e) => e.visit(v),
                Self::Dyn(e) => e.visit(v),
                Self::Data(e) => e.visit(v),
                Self::On(e) => e.visit(v),
            }
        }
    }

    impl<'a> Display for StateStmt<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Declaration(e) => write!(f, "{e}")?,
                Self::Transition(e) => write!(f, "{e}")?,
                Self::Dyn(e) => write!(f, "{e}")?,
                Self::Data(e) => write!(f, "{e}")?,
                Self::On(e) => write!(f, "{e}")?,
            }

            Ok(())
        }
    }

    /// A state-transition statement
    /// ex:
    /// ```comune
    /// =bail> error;
    /// ```
    #[derive(Debug)]
    pub struct StateTransition<'a> {
        pub id: NodeId,

        pub equal: EqualToken<'a>,
        pub name_id: IdenToken<'a>,
        pub caret_close: CaretCloseToken<'a>,
        pub target_id: IdenToken<'a>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node for StateTransition<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for StateTransition<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_state_transition(self, Prefix)?;

            v.visit_state_transition(self, Suffix)
        }
    }

    impl<'a> Display for StateTransition<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.equal)?;
            write!(f, "{}", self.name_id)?;
            write!(f, "{} ", self.caret_close)?;
            write!(f, "{}", self.target_id)?;
            write!(f, "{}", self.semi)?;

            Ok(())
        }
    }

    #[derive(Debug, From)]
    pub enum Declaration<'a> {
        Machine(MachineDeclaration<'a>),
        Let(LetDeclaration<'a>),
    }

    impl<'a> Visitable<'a> for Declaration<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::Machine(e) => e.visit(v),
                Self::Let(e) => e.visit(v),
            }
        }
    }
    
    impl<'a> Display for Declaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Machine(m) => write!(f, "{m}")?,
                Self::Let(m) => write!(f, "{m}")?,
            }

            Ok(())
        }
    }


    /// A dynamic statement
    #[derive(Debug)]
    pub struct Dyn<'a> {
        pub id: NodeId,

        pub src_expr: Expr<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr<'a>,
        /// Only optional if stmt is a block statement
        pub semi: Option<SemiColonToken<'a>>,
    }

    impl<'a> Node for Dyn<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for Dyn<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_dyn(self, Prefix)?;

            self.src_expr.visit(v)?;
            self.stmt.visit(v)?;

            v.visit_dyn(self, Suffix)
        }
    }

    impl<'a> Display for Dyn<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}",
                self.src_expr,
                self.thin_arrow,
                self.stmt
            )?;
            if let Some(semi) = &self.semi {
                write!(f, "{semi}")?;
            }

            Ok(())
        }
    }

    /// A state-data statement
    #[derive(Debug)]
    pub struct Data<'a> {
        pub id: NodeId,

        pub data: DataToken<'a>,
        pub curly_open: CurlyOpenToken<'a>,
        pub stmts: SeparatedList<'a, CurlyCloseToken<'a>, ComaToken<'a>, DataStmt<'a>>,
        pub curly_close: CurlyCloseToken<'a>,
    }

    impl<'a> Node for Data<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for Data<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_data(self, Prefix)?;

            self.stmts.visit(v)?;

            v.visit_data(self, Suffix)
        }
    }

    impl<'a> Display for Data<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {}{}{}",
                self.data,
                self.curly_open,
                self.stmts,
                self.curly_close
            )?;

            Ok(())
        }
    }

    /// Statements that go into a Data statement
    #[derive(Debug)]
    pub struct DataStmt<'a> {
        pub id: NodeId,

        pub mutability: Option<MutToken<'a>>,
        pub iden: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    impl<'a> Node for DataStmt<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for DataStmt<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_data_stmt(self, Prefix)?;

            v.visit_data_stmt(self, Suffix)
        }
    }

    impl<'a> Display for DataStmt<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(mut_) = &self.mutability {
                write!(f, "{mut_} ")?;
            }
            write!(f, "{} {}",
                self.iden, self.ty
            )?;

            Ok(())
        }
    }

    /// A 'on' statement
    #[derive(Debug)]
    pub struct On<'a> {
        pub id: NodeId,

        pub on: OnToken<'a>,
        pub iden: IdenToken<'a>,
        pub paren_open: ParenOpenToken<'a>,
        pub params: SeparatedList<'a, ParenCloseToken<'a>, ComaToken<'a>, Parameter<'a>>,
        pub paren_close: ParenCloseToken<'a>,
        pub body: BlockExpr<'a>,
    }

    impl<'a> Node for On<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for On<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_on(self, Prefix)?;

            self.params.visit(v)?;

            v.visit_on(self, Suffix)
        }
    }

    impl<'a> Display for On<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}{}{} {}",
                self.on, self.iden, self.paren_open, self.params,
                self.paren_close, self.body
            )?;

            Ok(())
        }
    }

    /// A parameter of a function / 'on' statement
    #[derive(Debug)]
    pub struct Parameter<'a> {
        pub id: NodeId,

        pub iden: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    impl<'a> Node for Parameter<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for Parameter<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_parameter(self, Prefix)?;

            v.visit_parameter(self, Suffix)
        }
    }

    impl<'a> Display for Parameter<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {}",
                self.iden, self.ty
            )?;

            Ok(())
        }
    }

    /// A Type
    #[derive(Debug, From)]
    pub enum Type<'a> {
        Named(IdenToken<'a>),
    }

    impl<'a> Display for Type<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Type::Named(n) => write!(f, "{n}")?,
            }

            Ok(())
        }
    }

    /// Operator in the infix form
    #[derive(Debug)]
    pub struct InfixOp<'a, Left, Op: KnownToken<'a>, Right> {
        pub id: NodeId,

        pub left: Box<Left>,
        pub op: Op,
        pub right: Box<Right>,
        pub p: PhantomData<*const &'a ()>,
    }

    impl<'a, Left, Op, Right> Node for InfixOp<'a, Left, Op, Right>
        where Op: KnownToken<'a>
    {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a, Left, Op, Right> Visitable<'a> for InfixOp<'a, Left, Op, Right>
        where Op: KnownToken<'a>
    {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_infix_op(self, Prefix)?;
            v.visit_infix_op(self, Suffix)
        }
    }

    impl<'a, Left, Op, Right> Display for InfixOp<'a, Left, Op, Right>
        where Left: Display,
              Op: KnownToken<'a> + Display,
              Right: Display
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.left)?;
            write!(f, " {} ", self.op)?;
            write!(f, "{}", self.right)?;

            Ok(())
        }
    }

    /// An expression surrounded by parenthesis
    #[derive(Debug)]
    pub struct ParentisedExpr<'a> {
        pub id: NodeId,

        pub open: ParenOpenToken<'a>,
        pub expr: Box<Expr<'a>>,
        pub close: ParenCloseToken<'a>,
    }

    impl<'a> Node for ParentisedExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for ParentisedExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_parentised(self, Prefix)?;

            self.expr.visit(v)?;

            v.visit_parentised(self, Suffix)
        }
    }

    impl<'a> Display for ParentisedExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}{}", self.open, self.expr, self.close)?;

            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct SemiedExpr<'a> {
        pub id: NodeId,

        pub expr: Box<Expr<'a>>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node for SemiedExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for SemiedExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_semied_expr(self, Prefix)?;
            self.expr.visit(v)?;
            v.visit_semied_expr(self, Suffix)
        }
    }

    impl<'a> Display for SemiedExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.expr, self.semi)?;

            Ok(())
        }
    }

    /// Statement that goes into a BlockExpr
    #[derive(From, Debug)]
    pub enum BlockStatement<'a> {
        Decl(Declaration<'a>),
        Expr(SemiedExpr<'a>),
    }

    impl<'a> Visitable<'a> for BlockStatement<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::Decl(e) => e.visit(v),
                Self::Expr(e) => e.visit(v),
            }
        }
    }

    impl<'a> Display for BlockStatement<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Decl(e) => write!(f, "{e}")?,
                Self::Expr(e) => write!(f, "{e}")?,
            }

            Ok(())
        }
    }

    /// A list of expressions separated by semicolons and surrounded by 
    /// curly braces
    #[derive(Debug)]
    pub struct BlockExpr<'a> {
        pub id: NodeId,

        pub open: CurlyOpenToken<'a>,

        pub stmts: Box<[BlockStatement<'a>]>,
        /// If there is an expression without semi colon at the end
        /// it is placed here
        pub last_expr: Option<Box<Expr<'a>>>,

        pub close: CurlyCloseToken<'a>,
    }

    impl<'a> Node for BlockExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for BlockExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_block(self, Prefix)?;

            for s in self.stmts.iter() {
                s.visit(v)?;
            }
            if let Some(le) = &self.last_expr {
                le.visit(v)?;
            }

            v.visit_block(self, Suffix)
        }
    }

    impl<'a> Display for BlockExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.open)?;
            for st in self.stmts.iter() {
                write!(f, "{st}")?;
            }
            if let Some(last) = &self.last_expr {
                write!(f, "{last}")?;
            }
            write!(f, "{}", self.close)?;

            Ok(())
        }
    }
    
    /// An if expression
    #[derive(Debug)]
    pub struct IfExpr<'a>{
        pub id: NodeId,

        pub if_kw: IfToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub then_kw: ThenToken<'a>,
        pub then: Box<Expr<'a>>,
        pub else_: Option<(ElseToken<'a>, Box<Expr<'a>>)>,
    }

    impl<'a> Node for IfExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for IfExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_if(self, Prefix)?;

            self.cond.visit(v)?;
            self.then.visit(v)?;

            if let Some((_, e)) = &self.else_ {
                e.visit(v)?;
            }

            v.visit_if(self, Suffix)
        }
    }

    impl<'a> Display for IfExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ", self.if_kw)?;
            write!(f, "{} ", self.cond)?;
            write!(f, "{} ", self.then_kw)?;
            write!(f, "{}", self.then)?;
            if let Some((else_, expr)) = &self.else_ {
                write!(f, " {else_} {expr}")?;
            }

            Ok(())
        }
    }

    /// A while expression
    #[derive(Debug)]
    pub struct WhileExpr<'a>{
        pub id: NodeId,

        pub while_: WhileToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub do_kw: DoToken<'a>,
        pub do_: Box<Expr<'a>>,
    }

    impl<'a> Node for WhileExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for WhileExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_while(self, Prefix)?;

            self.cond.visit(v);
            self.do_.visit(v);

            v.visit_while(self, Suffix)
        }
    }

    impl<'a> Display for WhileExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.while_)?;
            write!(f, " {}", self.cond)?;
            write!(f, " {}", self.do_kw)?;
            write!(f, " {}", self.do_)?;

            Ok(())
        }
    }

    #[derive(Debug, From)]
    pub struct DecimalLiteral<'a> {
        pub id: NodeId,
        pub tok: DecimalLiteralToken<'a>,
    }

    impl<'a> Node for DecimalLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for DecimalLiteral<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_decimal_literal(self, Prefix)?;

            v.visit_decimal_literal(self, Suffix)
        }
    }

    impl<'a> Display for DecimalLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, From)]
    pub struct StringLiteral<'a> {
        pub id: NodeId,
        pub tok: StringLiteralToken<'a>,
    }

    impl<'a> Node for StringLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for StringLiteral<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_string_literal(self, Prefix)?;
            v.visit_string_literal(self, Suffix)
        }
    }

    impl<'a> Display for StringLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, From)]
    pub struct TrueLiteral<'a> {
        pub id: NodeId,
        pub tok: TrueToken<'a>,
    }

    impl<'a> Node for TrueLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for TrueLiteral<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_true_literal(self, Prefix)?;
            v.visit_true_literal(self, Suffix)
        }
    }

    impl<'a> Display for TrueLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, From)]
    pub struct FalseLiteral<'a> {
        pub id: NodeId,
        pub tok: FalseToken<'a>,
    }

    impl<'a> Node for FalseLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for FalseLiteral<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_false_literal(self, Prefix)?;
            v.visit_false_literal(self, Suffix)
        }
    }

    impl<'a> Display for FalseLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    /// A literal value
    #[derive(Debug, From)]
    pub enum AnyLiteral<'a> {
        Decimal(DecimalLiteral<'a>),
        String(StringLiteral<'a>),
        True(TrueLiteral<'a>),
        False(FalseLiteral<'a>),
    }

    impl<'a> Visitable<'a> for AnyLiteral<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Self::Decimal(e) => e.visit(v),
                Self::String(e) => e.visit(v),
                Self::True(e) => e.visit(v),
                Self::False(e) => e.visit(v),
            }
        }
    }

    impl<'a> Display for AnyLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Decimal(e) => write!(f, "{e}"),
                Self::String(e) => write!(f, "{e}"),
                Self::True(e) => write!(f, "{e}"),
                Self::False(e) => write!(f, "{e}"),
            }
        }
    }

    #[derive(Debug)]
    pub struct FunctionApplicationExpr<'a> {
        pub id: NodeId,

        pub expr: Box<Expr<'a>>,
        pub arguments: Box<[Expr<'a>]>,
    }

    impl<'a> Node for FunctionApplicationExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for FunctionApplicationExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_function_application(self, Prefix)?;

            self.expr.visit(v)?;
            for e in self.arguments.iter() {
                e.visit(v)?;
            }

            v.visit_function_application(self, Suffix)
        }
    }

    impl<'a> Display for FunctionApplicationExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.expr)?;

            for arg in self.arguments.iter() {
                write!(f, " {arg}")?;
            }

            Ok(())
        }
    }

    #[derive(Debug, From)]
    pub struct IdenExpr<'a> {
        pub id: NodeId,

        pub tok: IdenToken<'a>,
    }

    impl<'a> Node for IdenExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for IdenExpr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_iden_expr(self, Prefix)?;
            v.visit_iden_expr(self, Suffix)
        }
    }

    impl<'a> Display for IdenExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    /// An expression with intemediate precedance
    /// Has the BooleanOr binary operator
    #[derive(Debug, From)]
    pub enum Expr<'a> {
        Assignment(InfixOp<'a, Expr<'a>, EqualToken<'a>, Expr<'a>>),

        BooleanOr(InfixOp<'a, Expr<'a>, DoubleVBarToken<'a>, Expr<'a>>),

        BooleanAnd(InfixOp<'a, Expr<'a>, DoubleAndToken<'a>, Expr<'a>>),

        Equality(InfixOp<'a, Expr<'a>, DoubleEqualToken<'a>, Expr<'a>>),
        Greater(InfixOp<'a, Expr<'a>, CaretCloseToken<'a>, Expr<'a>>),
        GreaterEqual(InfixOp<'a, Expr<'a>, CaretCloseEqualToken<'a>, Expr<'a>>),
        Lower(InfixOp<'a, Expr<'a>, CaretOpenToken<'a>, Expr<'a>>),
        LowerEqual(InfixOp<'a, Expr<'a>, CaretOpenEqualToken<'a>, Expr<'a>>),

        Plus(InfixOp<'a, Expr<'a>, PlusToken<'a>, Expr<'a>>),
        Minus(InfixOp<'a, Expr<'a>, DashToken<'a>, Expr<'a>>),

        Times(InfixOp<'a, Expr<'a>, StarToken<'a>, Expr<'a>>),
        Divide(InfixOp<'a, Expr<'a>, FSlashToken<'a>, Expr<'a>>),

        FunctionApplication(FunctionApplicationExpr<'a>),

        Parentised(ParentisedExpr<'a>),
        Block(BlockExpr<'a>),
        Id(IdenExpr<'a>),
        If(IfExpr<'a>),
        While(WhileExpr<'a>),
        Literal(AnyLiteral<'a>),
    }

    impl<'a> Visitable<'a> for Expr<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            match self {
                Expr::Assignment(e) => e.visit(v),
                Expr::BooleanOr(e) => e.visit(v),
                Expr::BooleanAnd(e) => e.visit(v),
                Expr::Equality(e) => e.visit(v),
                Expr::Greater(e) => e.visit(v),
                Expr::GreaterEqual(e) => e.visit(v),
                Expr::Lower(e) => e.visit(v),
                Expr::LowerEqual(e) => e.visit(v),
                Expr::Plus(e) => e.visit(v),
                Expr::Minus(e) => e.visit(v),
                Expr::Times(e) => e.visit(v),
                Expr::Divide(e) => e.visit(v),
                Expr::FunctionApplication(e) => e.visit(v),
                Expr::Parentised(e) => e.visit(v),
                Expr::Block(e) => e.visit(v),
                Expr::Id(e) => e.visit(v),
                Expr::If(e) => e.visit(v),
                Expr::While(e) => e.visit(v),
                Expr::Literal(e) => e.visit(v),
            }
        }
    }

    impl<'a> Display for Expr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Parentised(e) => write!(f, "{e}")?,
                Self::Block(e) => write!(f, "{e}")?,
                Self::Id(e) => write!(f, "{e}")?,
                Self::If(e) => write!(f, "{e}")?,
                Self::While(e) => write!(f, "{e}")?,
                Self::Literal(e) => write!(f, "{e}")?,

                Self::Assignment(e) => write!(f, "({e})")?,
                Self::BooleanOr(e) => write!(f, "({e})")?,
                Self::BooleanAnd(e) => write!(f, "({e})")?,
                Self::Equality(e) => write!(f, "({e})")?,
                Self::Greater(e) => write!(f, "({e})")?,
                Self::GreaterEqual(e) => write!(f, "({e})")?,
                Self::Lower(e) => write!(f, "({e})")?,
                Self::LowerEqual(e) => write!(f, "({e})")?,
                Self::Plus(e) => write!(f, "({e})")?,
                Self::Minus(e) => write!(f, "({e})")?,
                Self::Times(e) => write!(f, "({e})")?,
                Self::Divide(e) => write!(f, "({e})")?,
                Self::FunctionApplication(e) => write!(f, "({e})")?,
            }

            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct File<'a> {
        pub id: NodeId,

        pub declarations: Box<[Declaration<'a>]>,
        pub eof: EOFToken<'a>,
    }

    impl<'a> Node for File<'a> {
        fn id(&self) -> NodeId {
            self.id
        }
    }
    impl<'a> Visitable<'a> for File<'a> {
        fn visit<V: AstVisitor<'a>>(&'a self, v: &mut V) -> ControlFlow<()> {
            v.visit_file(self, Prefix)?;

            for d in self.declarations.iter() {
                d.visit(v)?;
            }

            v.visit_file(self, Suffix)
        }
    }

    impl<'a> Display for File<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for decl in self.declarations.iter() {
                write!(f, "{decl}\n")?;
            }
            write!(f, "{}", self.eof)?;

            Ok(())
        }
    }
}

use std::marker::PhantomData;

use ast::*;
use crate::lexer::*;

macro_rules! expected {
    ($( $x:ident ),*) => {
        [].into_iter()
        $(.chain(<$x>::expected_first()))*
    };
}

/// Peeks the next token and continue parsing on the first parsable that expects
/// it as its first token
macro_rules! l_one {
    (use None) => {
        $x::parse($tokens)
    };
    (use $ctx:expr, $x:ident) => {
        $x::parse($ctx)
    };
    (use $ctx:expr, $x:ident $expr:block) => {
        $expr
    };
    (
        $ctx: expr;
        $($t:path => $x:ident $($exps:block)?),+
    ) => {
        match $ctx.tokens.peek_expecteds(
            [].into_iter()
            $(.chain(<$x>::expected_first()))+
        )? {
            $(
            GenericToken { kind, .. } if $x::expects(kind) => {
                l_one!(use $ctx, $x $($exps)?).map($t)
            }
            )+

            _ => unreachable!(),
        }
    };
}

macro_rules! match_token {
    (
        $ctx: expr;

        $($t:ident @ $x:ident => $b:block)+
    ) => {
        match $ctx.tokens.expecteds([
            $($x::KIND),+
        ].into_iter())? {
            $(
            g @ GenericToken { kind, .. } if kind == $x::KIND => {
                let $t = $x::from_generic(g).unwrap();
                $b
            }
            )+

            _ => unreachable!()
        }
    };
}

/// Extension trait for TokenStream
trait Util<'a> {
    fn expected<T: KnownToken<'a>>(&mut self) -> Result<T, ParserError>;
    fn expecteds(
        &mut self,
        tys: impl Iterator<Item = TokenType> + Clone
    ) -> Result<GenericToken<'a>, ParserError>;

    fn peek_expected(&mut self, ty: TokenType) -> Result<&GenericToken<'a>, ParserError>;
    fn peek_expecteds(
        &mut self,
        tys: impl Iterator<Item = TokenType> + Clone
    ) -> Result<&GenericToken<'a>, ParserError>;
}

impl<'a> Util<'a> for TokenStream<'a> {
    fn expected<T: KnownToken<'a>>(&mut self) -> Result<T, ParserError> {
        self.expecteds(std::iter::once(T::KIND))
            .map(|b| T::from_generic(b).unwrap())
    }
    fn expecteds(
        &mut self,
        tys: impl Iterator<Item = TokenType> + Clone
    ) -> Result<GenericToken<'a>, ParserError> {
        let tok = self.peek()?;
        if tys.clone().any(|ty| ty == tok.kind) {
            return Ok(self.get()?);
        }
        Err(ParserError::UnexpetedToken {
            read: tok.to_static(),
            expected: tys.collect::<Vec<_>>().into_boxed_slice()
        })
    }

    fn peek_expected(
        &mut self, kind: TokenType
    ) -> Result<&GenericToken<'a>, ParserError> {
        self.peek_expecteds([kind].into_iter())
    }
    fn peek_expecteds(
        &mut self,
        tys: impl Iterator<Item = TokenType> + Clone
    ) -> Result<&GenericToken<'a>, ParserError> {
        let p = self.peek()?;
        if tys.clone().any(|ty| ty == p.kind) {
            return Ok(p);
        }
        Err(ParserError::UnexpetedToken {
            read: p.to_static(),
            expected: tys.collect::<Vec<_>>().into_boxed_slice()
        })
    }
}

pub struct ParseContext<'a> {
    pub tokens: TokenStream<'a>,
}

impl<'a> ParseContext<'a> {
    pub fn from_src(filename: &'a str, src: &'a str) -> Self {
        Self {
            tokens: TokenStream::new(filename, src),
        }
    }

    pub fn new_id(&mut self) -> NodeId {
        NodeId::next()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Unexpected token, received {read:?} expected {expected:?}")]
    UnexpetedToken {
        read: GenericToken<'static>,
        expected: Box<[TokenType]>,
    },
    #[error(transparent)]
    LexerError(#[from] LexerError),
}

pub trait Parsable<'a>: Sized {
    fn expects(e: &TokenType) -> bool {
        Self::expected_first().any(|t| &t == e)
    }

    fn expected_first() -> impl Iterator<Item = TokenType> + Clone;

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError>;
}

pub fn parse<'a, T: Parsable<'a>>(
    ctx: &mut ParseContext<'a>
) -> Result<T, ParserError> {
    T::parse(ctx)
}

impl<'a, T: KnownToken<'a>> Parsable<'a> for T {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        std::iter::once(T::KIND)
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        ctx.tokens.expected()
    }
}

impl<'a, End: KnownToken<'a>, T: Parsable<'a>> Parsable<'a> for StmtContainer<'a, End, T> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            SemiColonToken,
            CurlyOpenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mut stmts = vec![];
        match_token!(ctx;
            t @ SemiColonToken => {
                while ctx.tokens.peek()?.kind != End::KIND {
                    stmts.push(T::parse(ctx)?);
                }

                Ok(Self::UseSemi { semi: t, stmts: stmts.into_boxed_slice() })
            }
            t @ CurlyOpenToken => {
                while ctx.tokens.peek()?.kind != TokenType::CurlyClose {
                    stmts.push(T::parse(ctx)?);
                }
                let close = ctx.tokens.expected::<CurlyCloseToken>()?;

                Ok(Self::UseBrackets {
                    open: t,
                    stmts: stmts.into_boxed_slice(),
                    close
                })
            }
        )
    }
}

impl<'a, End, Sep, T> Parsable<'a> for SeparatedList<'a, End, Sep, T>
    where End: KnownToken<'a>,
          Sep: KnownToken<'a>,
          T:   Parsable<'a>
{
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            End,
            T
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mut stmts = Vec::new();
        let mut last = None;

        while ctx.tokens.peek_eq(End::KIND)?.is_none() {
            let stmt = T::parse(ctx)?;
            let coma = ctx.tokens.get_eq()?;

            if coma.is_none() || ctx.tokens.peek_eq(End::KIND)?.is_some() {
                last = Some((Box::new(stmt), coma));
                break;
            }

            stmts.push((
                stmt,
                coma.unwrap(),
            ));
        }
        ctx.tokens.peek_expected(End::KIND)?;

        Ok(Self {
            phantom: std::marker::PhantomData,
            stmts: stmts.into_boxed_slice(),
            last,
        })
    }
}

impl<'a> Parsable<'a> for MachineDeclaration<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        [TokenType::Machine].into_iter()
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let machine_token = ctx.tokens.expected::<MachineToken>()?;
        let iden = ctx.tokens.expected::<IdenToken>()?;
        let stmts = StmtContainer::parse(ctx)?;

        Ok(MachineDeclaration {
            id: ctx.new_id(),

            machine_token,
            iden,
            stmts,
        })
    }
}

impl<'a> Parsable<'a> for MachineStmt<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Declaration,
            StateDeclaration
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Declaration => Declaration,
            Self::State => StateDeclaration
        )
    }
}

impl<'a> Parsable<'a> for StateDeclaration<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            InitialToken,
            StateToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let initial_token = ctx.tokens.get_eq::<InitialToken>()?;
        let state_token = ctx.tokens.expected::<StateToken>()?;
        let iden = ctx.tokens.expected::<IdenToken>()?;
        let stmts = StmtContainer::parse(ctx)?;

        Ok(StateDeclaration {
            id: ctx.new_id(),

            initial_token,
            state_token,
            iden,
            stmts,
        })
    }
}

impl<'a> Parsable<'a> for StateStmt<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Declaration,
            StateTransition,
            Data,
            On,
            Dyn
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;

            Self::Declaration => Declaration,
            Self::Transition => StateTransition,
            Self::Data => Data,
            Self::On => On,
            Self::Dyn => Dyn
        )
    }
}

impl<'a> Parsable<'a> for StateTransition<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            EqualToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(StateTransition {
            id: ctx.new_id(),

            equal: parse(ctx)?,
            name_id: parse(ctx)?,
            caret_close: parse(ctx)?,
            target_id: parse(ctx)?,
            semi: parse(ctx)?,
        })
    }
}

impl<'a> Parsable<'a> for LetDeclaration<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            LetToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let let_ = parse(ctx)?;
        let mut_ = ctx.tokens.get_eq()?;
        let name = parse(ctx)?;
        let mut params = Vec::new();
        while ctx.tokens.peek_eq(TokenType::Equal)?.is_none() {
            params.push(parse(ctx)?);
        }
        let eq = parse(ctx)?;
        let expr = parse(ctx)?;
        let semi = parse(ctx)?;

        Ok(Self {
            id: ctx.new_id(),

            let_,
            mut_,
            name,
            params: params.into_boxed_slice(),
            eq,
            expr,
            semi,
        })
    }
}

impl<'a> Parsable<'a> for Declaration<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            MachineDeclaration,
            LetDeclaration
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Machine => MachineDeclaration,
            Self::Let => LetDeclaration
        )
    }
}

impl<'a> Parsable<'a> for Dyn<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let src_expr = Expr::parse(ctx)?;
        let thin_arrow = ctx.tokens.expected::<ThinArrowToken>()?;
        let stmt = Expr::parse(ctx)?;
        let is_block = matches!(stmt, Expr::Block(..));
        let semi = if is_block { ctx.tokens.get_eq()? }
            else { Some(ctx.tokens.expected()?) };

        Ok(Self {
            id: ctx.new_id(),

            src_expr,
            thin_arrow,
            stmt,
            semi,
        })
    }
}

impl<'a> Parsable<'a> for Data<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            DataToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let data = ctx.tokens.expected::<DataToken>()?;
        let curly_open = ctx.tokens.expected::<CurlyOpenToken>()?;
        let stmts = SeparatedList::parse(ctx)?;
        let curly_close = ctx.tokens.expected::<CurlyCloseToken>()?;

        Ok(Self {
            id: ctx.new_id(),

            data,
            curly_open,
            stmts,
            curly_close,
        })
    }
}

impl<'a> Parsable<'a> for DataStmt<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            MutToken,
            IdenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mutability = ctx.tokens.get_eq::<MutToken>()?;
        let iden = ctx.tokens.expected::<IdenToken>()?;
        let ty = Type::parse(ctx)?;

        Ok(Self {
            id: ctx.new_id(),

            mutability,
            iden,
            ty,
        })
    }
}

impl<'a> Parsable<'a> for DecimalLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            DecimalLiteralToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),

            tok: ctx.tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for StringLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            StringLiteralToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),

            tok: ctx.tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for TrueLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            TrueToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),

            tok: ctx.tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for FalseLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            FalseToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),

            tok: ctx.tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for On<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            OnToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let on = ctx.tokens.expected::<OnToken>()?;
        let iden = ctx.tokens.expected::<IdenToken>()?;
        let paren_open = ctx.tokens.expected::<ParenOpenToken>()?;
        let params = SeparatedList::parse(ctx)?;
        let paren_close = ctx.tokens.expected::<ParenCloseToken>()?;
        let body = BlockExpr::parse(ctx)?;

        Ok(Self {
            id: ctx.new_id(),

            on,
            iden,
            paren_open,
            params,
            paren_close,
            body,
        })
    }
}

impl<'a> Parsable<'a> for AnyLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            StringLiteral,
            DecimalLiteral,
            TrueLiteral,
            FalseLiteral
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::String => StringLiteral,
            Self::Decimal => DecimalLiteral,
            Self::False => FalseLiteral,
            Self::True => TrueLiteral
        )
    }
}

impl<'a> Parsable<'a> for IdenExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            IdenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),
            tok: ctx.tokens.expected()?,
        })
    }
}

impl<'a> Expr<'a> {
    fn bottom_parse(
        ctx: &mut ParseContext<'a>
    ) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Parentised => ParentisedExpr,
            Self::Block => BlockExpr,
            Self::Id => IdenExpr,
            Self::If => IfExpr,
            Self::While => WhileExpr,
            Self::Literal => AnyLiteral
        )
    }

    const ASSIGNMENT_PRECEDENCE: usize = 0;

    const BOOLEAN_OR_PRECEDENCE: usize = 1;

    const BOOLEAN_AND_PRECEDENCE: usize = 2;

    const EQUALITY_PRECEDENCE: usize = 3;
    const LOWER_PRECEDENCE: usize = 3;
    const LOWER_EQUAL_PRECEDENCE: usize = 3;
    const GREATER_PRECEDENCE: usize = 3;
    const GREATER_EQUAL_PRECEDENCE: usize = 3;

    const PLUS_PRECEDENCE: usize = 4;
    const MINUS_PRECEDENCE: usize = 4;

    const TIMES_PRECEDENCE: usize = 5;
    const DIVIDE_PRECEDENCE: usize = 5;

    const FUNCTION_APPLICATION_PRECEDENCE: usize = 6;

    const BOTTOM_PRECEDENCE: usize = 7;

    fn expr_parse(
        ctx: &mut ParseContext<'a>, precedence: usize
    ) -> Result<Self, ParserError> {
        if precedence >= Self::BOTTOM_PRECEDENCE {
            return Self::bottom_parse(ctx);
        }

        let mut left = Self::expr_parse(ctx, precedence + 1)?;

        use TokenType as TT;

        macro_rules! take_precedence {
            ($($tt:path => $k:path [$p:expr],)*) => {
                match ctx.tokens.peek()?.kind {
                $($tt if precedence == $p => {
                    left = $k(InfixOp {
                        id: ctx.new_id(),

                        left: Box::new(left),
                        op: ctx.tokens.expected()?,
                        right: Box::new(Expr::expr_parse(ctx, precedence + 1)?),
                        p: PhantomData,
                    });
                    continue;
                },)*

                _ => (),
                }
            };
        }

        loop {
            // If the next token is the specified token and the precedence
            // matches, continue the left-associated operation
            // 'continue' the loop if a token is matched
            take_precedence!(
                TT::Equal          
                    => Self::Assignment   [Self::ASSIGNMENT_PRECEDENCE],

                TT::DoubleVBar     
                    => Self::BooleanOr    [Self::BOOLEAN_OR_PRECEDENCE],

                TT::DoubleAnd      
                    => Self::BooleanAnd   [Self::BOOLEAN_AND_PRECEDENCE],

                TT::DoubleEqual    
                    => Self::Equality     [Self::EQUALITY_PRECEDENCE],
                TT::CaretOpen      
                    => Self::Lower        [Self::LOWER_PRECEDENCE],
                TT::CaretOpenEqual 
                    => Self::LowerEqual   [Self::LOWER_EQUAL_PRECEDENCE],
                TT::CaretClose     
                    => Self::Greater      [Self::GREATER_PRECEDENCE],
                TT::CaretCloseEqual
                    => Self::GreaterEqual [Self::GREATER_EQUAL_PRECEDENCE],

                TT::Plus           
                    => Self::Plus         [Self::PLUS_PRECEDENCE],
                TT::Dash           
                    => Self::Minus        [Self::MINUS_PRECEDENCE],

                TT::Star           
                    => Self::Times        [Self::TIMES_PRECEDENCE],
                TT::FSlash         
                    => Self::Divide       [Self::DIVIDE_PRECEDENCE],
            );

            // Function calls after this
            if precedence != Self::FUNCTION_APPLICATION_PRECEDENCE {
                break;
            }

            // Not an expression -> no function application
            if !Expr::expects(&ctx.tokens.peek()?.kind) {
                break;
            }

            let mut arguments = vec![];
            while Expr::expects(&ctx.tokens.peek()?.kind) {
                arguments.push(Expr::expr_parse(ctx, precedence + 1)?);
            }
            left = Expr::FunctionApplication(FunctionApplicationExpr {
                id: ctx.new_id(),
                expr: Box::new(left),
                arguments: arguments.into_boxed_slice(),
            });
        }
        return Ok(left);
    }
}

impl<'a> Parsable<'a> for Expr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParentisedExpr,
            BlockExpr,
            IdenExpr,
            IfExpr,
            WhileExpr,
            AnyLiteral
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Self::expr_parse(ctx, 0)
    }
}

impl<'a> Parsable<'a> for ParentisedExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParenOpenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(ParentisedExpr {
            id: ctx.new_id(),

            open: parse(ctx)?,
            expr: Box::new(parse(ctx)?),
            close: parse(ctx)?,
        })
    }
}

impl<'a> Parsable<'a> for FunctionApplicationExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let expr = Expr::expr_parse(ctx, Expr::FUNCTION_APPLICATION_PRECEDENCE)?;

        let mut arguments = vec![];
        while Expr::expects(&ctx.tokens.peek()?.kind) {
            arguments.push(
                Expr::expr_parse(ctx, Expr::FUNCTION_APPLICATION_PRECEDENCE)?
            );
        }

        Ok(Self {
            id: ctx.new_id(),
            expr: Box::new(expr),
            arguments: arguments.into_boxed_slice(),
        })
    }
}

impl<'a> Parsable<'a> for BlockExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            CurlyOpenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let open = parse(ctx)?;

        enum ExprOrDecl<'a> {
            Expr(Expr<'a>),
            Decl(Declaration<'a>),
        }

        let mut stmts = Vec::new();
        let mut last_expr = None;

        while ctx.tokens.peek_eq(TokenType::CurlyClose)?.is_none() {
            let stmt = l_one!(ctx;
                ExprOrDecl::Expr => Expr,
                ExprOrDecl::Decl => Declaration
            )?;

            match stmt {
                ExprOrDecl::Decl(d) => {
                    stmts.push(BlockStatement::Decl(d));
                },
                ExprOrDecl::Expr(e)
                    if ctx.tokens.peek_eq(TokenType::SemiColon)?.is_some() =>
                {
                    stmts.push(BlockStatement::Expr(SemiedExpr {
                        id: ctx.new_id(),

                        expr: Box::new(e),
                        semi: parse(ctx)?,
                    }));
                },
                // Expr with no semi colon = end of block
                ExprOrDecl::Expr(e) => {
                    last_expr = Some(Box::new(e));
                    break
                }
            }
        }

        let close = parse(ctx)?;

        Ok(BlockExpr {
            id: ctx.new_id(),

            open,
            stmts: stmts.into_boxed_slice(),
            last_expr,
            close,
        })
    }
}

impl<'a> Parsable<'a> for Type<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            IdenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Named => IdenToken
        )
    }
}

impl<'a> Parsable<'a> for Parameter<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            IdenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: ctx.new_id(),

            iden: parse(ctx)?,
            ty: parse(ctx)?,
        })
    }
}

impl<'a> Parsable<'a> for IfExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            IfToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let if_ = ctx.tokens.expected::<IfToken>()?;
        let cond = Expr::parse(ctx)?;

        let then_kw = ctx.tokens.expected::<ThenToken>()?;
        let then = Expr::parse(ctx)?;

        let mut else_ = None::<(ElseToken<'a>, Box<Expr<'a>>)>;
        if ctx.tokens.peek_eq(TokenType::Else)?.is_some() {
            let tok = ctx.tokens.expected::<ElseToken>()?;
            let expr;
            if ctx.tokens.peek_eq(TokenType::If)?.is_some() {
                expr = Expr::If(parse(ctx)?);
            }
            else {
                expr = Expr::parse(ctx)?;
            }
            else_ = Some((tok, Box::new(expr.into())));
        }

        Ok(Self {
            id: ctx.new_id(),

            if_kw: if_,
            cond: Box::new(cond),
            then_kw,
            then: Box::new(then),
            else_,
        })
    }
}

impl<'a> Parsable<'a> for WhileExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            WhileToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let while_ = ctx.tokens.expected()?;

        let cond = Expr::parse(ctx)?;
        let do_kw = ctx.tokens.expected()?;
        let do_ = Expr::parse(ctx)?;

        Ok(Self {
            id: ctx.new_id(),

            while_,
            cond: Box::new(cond),
            do_kw,
            do_: Box::new(do_),
        })
    }
}

impl<'a> Parsable<'a> for File<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            EOFToken,
            Declaration
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mut decls = vec![];

        while ctx.tokens.peek_eq(TokenType::EOF)?.is_none() {
            decls.push(parse(ctx)?);
        }
        let eof = ctx.tokens.expected::<EOFToken>()?;

        Ok(Self {
            id: ctx.new_id(),

            declarations: decls.into_boxed_slice(),
            eof,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn expr_simple() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "5 + 5 / 5 + x * 5");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "((5 + (5 / 5)) + (x * 5))");

        Ok(())
    }

    #[test]
    fn expr_if() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "if x == 10 then 5 + 5");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "if (x == 10) then (5 + 5)");

        Ok(())
    }

    #[test]
    fn expr_if_else() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "if a + 5 == 10 then 5 + 5 else 0");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "if ((a + 5) == 10) then (5 + 5) else 0");

        Ok(())
    }

    #[test]
    fn expr_if_else_if() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "if (a + 5) == 10 then 5 + 5 else if true then 1 else 0");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "if (((a + 5)) == 10) then (5 + 5) else if true then 1 else 0");

        Ok(())
    }

    #[test]
    fn expr_while() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "while true do test");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "while true do test");

        Ok(())
    }

    #[test]
    fn block() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "{ a = 10; c = true && false; b = a + c; }");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "{(a = 10);(c = (true && false));(b = (a + c));}");

        Ok(())
    }

    #[test]
    fn block2() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "{ a = 10; c = true && false; b = a + c }");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "{(a = 10);(c = (true && false));(b = (a + c))}");

        Ok(())
    }

    #[test]
    fn expr_function_application() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "(this_is_a_function 5 (5 5 5) (10 + 10))");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "((this_is_a_function 5 ((5 5 5)) ((10 + 10))))");

        Ok(())
    }

    #[test]
    fn machine_curly_full() -> Result<(), Box<dyn Error>> {
        let src = r#"
        machine name {
            initial state first {
                data {
                    name string,
                    id u32,
                    mut received i32
                }

                =increment> second;

                reveived >= 10 -> {};

                on message(sender u32) {
                    a = 10;
                }
            }

            state second {
                =decrement> first;
            }
        }
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = MachineDeclaration::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "machine name { initial state first { data {name string,id u32,mut received i32} =increment> second; (reveived >= 10) -> {}; on message (sender u32) {(a = 10);} } state second { =decrement> first; } }");

        Ok(())
    }

    #[test]
    fn machine_in_machine() -> Result<(), Box<dyn Error>> {
        let src = r#"
        machine name {
            machine inner {

            }

            initial state first {
                machine even_more_inner {
                }

                data {}
            }
        }
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = MachineDeclaration::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "machine name { machine inner { } initial state first { machine even_more_inner { } data {} } }");

        Ok(())
    }

    #[test]
    fn let_alone() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let test = 5;
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = LetDeclaration::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "let test = 5;");

        Ok(())
    }

    #[test]
    fn let_alone_fn() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let plus a b = a + b;
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = LetDeclaration::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "let plus a b = (a + b);");

        Ok(())
    }

    #[test]
    fn let_alone_fn_block() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let plus a b = {
            let c = a + b;

            a * c
        };
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = LetDeclaration::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "let plus a b = {let c = (a + b);(a * c)};");

        Ok(())
    }
}
