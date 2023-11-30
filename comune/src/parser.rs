
pub mod ast {
    use std::marker::PhantomData;
    use std::fmt::{ Debug, Display };
    use std::ops::ControlFlow;
    use std::sync::atomic::AtomicU64;

    use crate::lexer::*;

    use macros::TryAs;
    use num_enum::{ TryFromPrimitive, IntoPrimitive };

    /// Unique ID (at least for the current execution) to identify nodes
    /// Should be created through the [super::ParseContext::new_id] method
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

    /// Trait implemented by all containers of [Node]s that themselfes
    /// are not [Node]s, also auto implemented for all [Node]s
    pub trait NodeContainer<'a> {
        /// Visit all [Node]s in the container
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V);

        /// Returns the only id contained by the container
        /// if there is more than one or no nodes in the container
        /// None is returned
        fn try_cont_id(&'a self) -> Option<NodeId> {
            #[derive(Default)]
            struct FirstVisitor {
                found: Option<NodeId>,
                found_more: bool,
            }

            impl<'b> AstVisitor<'b> for FirstVisitor {
                fn visit_any(
                    &mut self, node_ref: AnyNodeRef<'b>
                ) {
                    if self.found.is_some() {
                        self.found_more = true;
                    }
                    self.found = Some(node_ref.any_id());
                }
            }

            let mut v = FirstVisitor::default();
            self.container_visit(&mut v);
            (!v.found_more).then_some(()).and(v.found)
        }

        /// Unwraped version of [NodeContainer::try_cont_id]
        fn cont_id(&'a self) -> NodeId {
            self.try_cont_id().expect("Not a single element container")
        }
    }

    /// Trait of all nodes in the AST
    pub trait Node<'a> {
        /// A unique id for the node
        fn id(&self) -> NodeId;

        /// Visits all children nodes without revisiting the current Node
        /// nor visiting any grand children (i.e. all nodes at depth 1) 
        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V);
    }

    impl<'a, T> NodeContainer<'a> for T
        where T: Node<'a> + 'a,
              AnyNodeRef<'a>: From<&'a Self>,
    {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            v.visit_n(self)
        }
    }

    pub trait AstVisitor<'a>: Sized {
        /// Called for any walked node
        /// For full ast traversal use [Node::walk]
        /// (and/or [AnyNodeRef::any_walk]) to visit children
        fn visit_any(
            &mut self, node_ref: AnyNodeRef<'a>
        );

        /// Default impl is to call visit_any
        /// this is only a helper method for calling [AnyNodeRef]::from on
        /// the node ref
        fn visit_n<N>(
            &mut self, node: &'a N
        )
            where AnyNodeRef<'a>: From<&'a N>
        {
            self.visit_any(node.into())
        }
    }

    macro_rules! any_node {
        ($kind_name:ident;$name:ident;$($f:ident($type:ty),)*) => {
            #[derive(Debug, Clone, Copy, TryFromPrimitive, IntoPrimitive, PartialEq, Eq)]
            #[repr(u8)]
            pub enum $kind_name {
                $($f),*
            }

            #[derive(Debug, Clone, Copy, TryAs)]
            pub enum $name<'a> {
                $($f($type)),*
            }

            impl<'a> NodeContainer<'a> for $name<'a> {
                fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
                    match self {
                        $($name::$f(d) => d.container_visit(v),)*
                    }
                }
            }

            impl<'a> $name<'a> {
                pub fn kind(&self) -> $kind_name {
                    match self {
                        $($name::$f(..) => $kind_name::$f,)*
                    }
                }

                pub fn any_walk<V: AstVisitor<'a>>(&self, v: &mut V) {
                    match self {
                        $($name::$f(d) => d.walk(v),)*
                    }
                }
        
                pub fn any_id(&self) -> NodeId {
                    match self {
                        $($name::$f(d) => d.id(),)*
                    }
                }
            }

        };
    }

    any_node!(AnyNodeKind; AnyNodeRef;
        Infix(&'a InfixOp<'a>),
        LetBinding(&'a LetBinding<'a>),
        MachineTypeExpression(&'a MachineTypeExpression<'a>),
        StateDeclaration(&'a StateDeclaration<'a>),
        StateTransition(&'a StateTransition<'a>),
        Dyn(&'a Dyn<'a>),
        Data(&'a Data<'a>),
        DataItem(&'a DataItem<'a>),
        ParentisedExpr(&'a ParentisedExpr<'a>),
        SemiedExpr(&'a SemiedExpr<'a>),
        BlockExpr(&'a BlockExpr<'a>),
        IfExpr(&'a IfExpr<'a>),
        WhileExpr(&'a WhileExpr<'a>),
        DecimalLiteral(&'a DecimalLiteral<'a>),
        StringLiteral(&'a StringLiteral<'a>),
        TrueLiteral(&'a TrueLiteral<'a>),
        FalseLiteral(&'a FalseLiteral<'a>),
        FunctionApplicationExpr(&'a FunctionApplicationExpr<'a>),
        IdenExpr(&'a IdenExpr<'a>),
        File(&'a File<'a>),
    );

    /// List of nodes separated by a token, usually a comma (ex. arguments)
    /// with optional trailing separator
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct SeparatedList<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T> {
        pub phantom: PhantomData<*const &'a End>,

        pub items: Box<[(T, Sep)]>,
        /// Last statement is the only one with optional separator
        pub last: Option<(Box<T>, Option<Sep>)>,
    }

    impl<'a, End, Sep, T> NodeContainer<'a> for SeparatedList<'a, End, Sep, T>
        where End: KnownToken<'a> + Display,
              Sep: KnownToken<'a> + Display,
              T: NodeContainer<'a>
    {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            for (s, _) in self.items.iter() {
                s.container_visit(v);
            }
        }
    }

    impl<'a, End, Sep, T> Display for SeparatedList<'a, End, Sep, T>
        where End: KnownToken<'a> + Display,
              Sep: KnownToken<'a> + Display,
              T: Display
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for (t, sep) in self.items.iter() {
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

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct LetBinding<'a> {
        pub id: NodeId,

        pub let_: LetToken<'a>,
        pub mut_: Option<MutToken<'a>>,
        pub name: IdenToken<'a>,
        pub params: Box<[IdenToken<'a>]>,
        pub eq: EqualToken<'a>,
        pub expr: Expr<'a>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node<'a> for LetBinding<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.expr.container_visit(v)
        }
    }

    impl<'a> Display for LetBinding<'a> {
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

    /// A Machine type
    /// Ex:
    /// ```comune
    /// machine {
    ///     initial state init {
    ///        [...]
    ///     }
    ///
    ///     state second {
    ///        [...]
    ///     }
    /// }
    /// ```
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct MachineTypeExpression<'a> {
        pub id: NodeId,

        pub machine_token: MachineToken<'a>,
        pub open: CurlyOpenToken<'a>,
        pub items: Box<[MachineItem<'a>]>,
        pub close: CurlyCloseToken<'a>,
    }

    impl<'a> Node<'a> for MachineTypeExpression<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            for i in self.items.iter() {
                i.container_visit(v);
            }
        }
    }

    impl<'a> Display for MachineTypeExpression<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ", self.machine_token)?;
            write!(f, "{}", self.open)?;
            for i in self.items.iter() {
                write!(f, " {}", i)?;
            }
            write!(f, " {}", self.close)?;

            Ok(())
        }
    }

    /// Items that go into a Machine
    #[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
    pub enum MachineItem<'a> {
        Let(LetBinding<'a>),
        State(StateDeclaration<'a>),
    }

    impl<'a> NodeContainer<'a> for MachineItem<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                Self::Let(e) => e.container_visit(v),
                Self::State(e) => e.container_visit(v),
            }
        }
    }

    impl<'a> Display for MachineItem<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                MachineItem::Let(s) => write!(f, "{s}")?,
                MachineItem::State(s) => write!(f, "{s}")?,
            }

            Ok(())
        }
    }

    /// A State declaration
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct StateDeclaration<'a> {
        pub id: NodeId,

        pub initial_token: Option<InitialToken<'a>>,
        pub state_token: StateToken<'a>,
        pub iden: Option<IdenToken<'a>>,
        pub open: CurlyOpenToken<'a>,
        pub items: Box<[StateItem<'a>]>,
        pub close: CurlyCloseToken<'a>,
    }

    impl<'a> Node<'a> for StateDeclaration<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            for i in self.items.iter() {
                i.container_visit(v);
            }
        }
    }

    impl<'a> Display for StateDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(initial) = &self.initial_token {
                write!(f, "{} ", initial)?;
            }
            write!(f, "{} ", self.state_token)?;
            if let Some(iden) = self.iden {
                write!(f, "{iden} ")?;
            }
            write!(f, "{}", self.open)?;
            for i in self.items.iter() {
                write!(f, " {}", i)?;
            }
            write!(f, " {}", self.close)?;

            Ok(())
        }
    }

    /// Itemss that go into a State
    #[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
    pub enum StateItem<'a> {
        Let(LetBinding<'a>),
        Transition(StateTransition<'a>),
        Dyn(Dyn<'a>),
        Data(Data<'a>),
    }

    impl<'a> NodeContainer<'a> for StateItem<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                Self::Let(e) => e.container_visit(v),
                Self::Transition(e) => e.container_visit(v),
                Self::Dyn(e) => e.container_visit(v),
                Self::Data(e) => e.container_visit(v),
            }
        }
    }

    impl<'a> Display for StateItem<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Let(e) => write!(f, "{e}")?,
                Self::Transition(e) => write!(f, "{e}")?,
                Self::Dyn(e) => write!(f, "{e}")?,
                Self::Data(e) => write!(f, "{e}")?,
            }

            Ok(())
        }
    }

    /// A state-transition statement
    /// ex:
    /// ```comune
    /// =bail> error;
    /// ```
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct StateTransition<'a> {
        pub id: NodeId,

        pub arrow: FatArrowToken<'a>,
        pub name_id: IdenToken<'a>,
        pub target_id: IdenToken<'a>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node<'a> for StateTransition<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for StateTransition<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.arrow)?;
            write!(f, " {}", self.name_id)?;
            write!(f, " {}", self.target_id)?;
            write!(f, "{}", self.semi)?;

            Ok(())
        }
    }

    /// A dynamic statement
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Dyn<'a> {
        pub id: NodeId,

        pub src_expr: Expr<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr<'a>,
        /// Only optional if stmt is a block statement
        pub semi: Option<SemiColonToken<'a>>,
    }

    impl<'a> Node<'a> for Dyn<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.src_expr.container_visit(v);
            self.stmt.container_visit(v);
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
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Data<'a> {
        pub id: NodeId,

        pub data: DataToken<'a>,
        pub curly_open: CurlyOpenToken<'a>,
        pub items: SeparatedList<'a, CurlyCloseToken<'a>, ComaToken<'a>, DataItem<'a>>,
        pub curly_close: CurlyCloseToken<'a>,
    }

    impl<'a> Node<'a> for Data<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.items.container_visit(v);
        }
    }

    impl<'a> Display for Data<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {}{}{}",
                self.data,
                self.curly_open,
                self.items,
                self.curly_close
            )?;

            Ok(())
        }
    }

    /// Items that go into a Data statement
    /// aka `[mut ]name: ty`
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct DataItem<'a> {
        pub id: NodeId,

        pub mutability: Option<MutToken<'a>>,
        pub iden: IdenToken<'a>,
        pub ty: Expr<'a>,
    }

    impl<'a> Node<'a> for DataItem<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for DataItem<'a> {
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

    /// Operator in the infix form
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct InfixOp<'a> {
        pub id: NodeId,

        pub left: Box<Expr<'a>>,
        pub op: InfixIdenToken<'a>,
        pub right: Box<Expr<'a>>,
    }

    impl<'a> Node<'a> for InfixOp<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.left.container_visit(v);
            self.right.container_visit(v);
        }
    }

    impl<'a> Display for InfixOp<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.left)?;
            write!(f, " {} ", self.op)?;
            write!(f, "{}", self.right)?;

            Ok(())
        }
    }

    /// An expression surrounded by parenthesis
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ParentisedExpr<'a> {
        pub id: NodeId,

        pub open: ParenOpenToken<'a>,
        pub expr: Box<Expr<'a>>,
        pub close: ParenCloseToken<'a>,
    }

    impl<'a> Node<'a> for ParentisedExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.expr.container_visit(v);
        }
    }

    impl<'a> Display for ParentisedExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}{}", self.open, self.expr, self.close)?;

            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct SemiedExpr<'a> {
        pub id: NodeId,

        pub expr: Box<Expr<'a>>,
        pub semi: SemiColonToken<'a>,
    }

    impl<'a> Node<'a> for SemiedExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.expr.container_visit(v);
        }
    }

    impl<'a> Display for SemiedExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.expr, self.semi)?;

            Ok(())
        }
    }

    /// Items that goes into a BlockExpr
    #[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
    pub enum BlockItem<'a> {
        Let(LetBinding<'a>),
        Expr(SemiedExpr<'a>),
    }

    impl<'a> NodeContainer<'a> for BlockItem<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                Self::Let(e) => e.container_visit(v),
                Self::Expr(e) => e.container_visit(v),
            }
        }
    }

    impl<'a> Display for BlockItem<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Let(e) => write!(f, "{e}")?,
                Self::Expr(e) => write!(f, "{e}")?,
            }

            Ok(())
        }
    }

    /// A list of expressions separated by semicolons and surrounded by 
    /// curly braces
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct BlockExpr<'a> {
        pub id: NodeId,

        pub open: CurlyOpenToken<'a>,

        pub items: Box<[BlockItem<'a>]>,
        /// If there is an expression without semi colon at the end
        /// it is placed here
        pub last_expr: Option<Box<Expr<'a>>>,

        pub close: CurlyCloseToken<'a>,
    }

    impl<'a> Node<'a> for BlockExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            for i in self.items.iter() {
                i.container_visit(v);
            }
            if let Some(le) = &self.last_expr {
                le.container_visit(v);
            }
        }
    }

    impl<'a> Display for BlockExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.open)?;
            for st in self.items.iter() {
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
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct IfExpr<'a>{
        pub id: NodeId,

        pub if_kw: IfToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub then_kw: ThenToken<'a>,
        pub then: Box<Expr<'a>>,
        pub else_: Option<(ElseToken<'a>, Box<Expr<'a>>)>,
    }

    impl<'a> Node<'a> for IfExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.cond.container_visit(v);
            self.then.container_visit(v);

            if let Some((_, e)) = &self.else_ {
                e.container_visit(v);
            }
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
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct WhileExpr<'a>{
        pub id: NodeId,

        pub while_: WhileToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub do_kw: DoToken<'a>,
        pub do_: Box<Expr<'a>>,
    }

    impl<'a> Node<'a> for WhileExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.cond.container_visit(v);
            self.do_.container_visit(v);
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

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct DecimalLiteral<'a> {
        pub id: NodeId,
        pub tok: DecimalLiteralToken<'a>,
    }

    impl<'a> Node<'a> for DecimalLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for DecimalLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct StringLiteral<'a> {
        pub id: NodeId,
        pub tok: StringLiteralToken<'a>,
    }

    impl<'a> Node<'a> for StringLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for StringLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct TrueLiteral<'a> {
        pub id: NodeId,
        pub tok: TrueToken<'a>,
    }

    impl<'a> Node<'a> for TrueLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for TrueLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct FalseLiteral<'a> {
        pub id: NodeId,
        pub tok: FalseToken<'a>,
    }

    impl<'a> Node<'a> for FalseLiteral<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for FalseLiteral<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    /// A literal value
    #[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
    pub enum AnyLiteral<'a> {
        Decimal(DecimalLiteral<'a>),
        String(StringLiteral<'a>),
        True(TrueLiteral<'a>),
        False(FalseLiteral<'a>),
    }

    impl<'a> NodeContainer<'a> for AnyLiteral<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                Self::Decimal(e) => e.container_visit(v),
                Self::String(e) => e.container_visit(v),
                Self::True(e) => e.container_visit(v),
                Self::False(e) => e.container_visit(v),
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

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct FunctionApplicationExpr<'a> {
        pub id: NodeId,

        pub callee: Box<Expr<'a>>,
        pub argument: Box<Expr<'a>>,
    }

    impl<'a> Node<'a> for FunctionApplicationExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            self.callee.container_visit(v);
            self.argument.container_visit(v);
        }
    }

    impl<'a> Display for FunctionApplicationExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.callee)?;
            write!(f, " {}", self.argument)?;

            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct IdenExpr<'a> {
        pub id: NodeId,

        pub tok: IdenToken<'a>,
    }

    impl<'a> Node<'a> for IdenExpr<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, _v: &mut V) { }
    }

    impl<'a> Display for IdenExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.tok)
        }
    }

    /// An expression with intemediate precedance
    /// Has the BooleanOr binary operator
    #[derive(TryAs, Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Expr<'a> {
        Infix(InfixOp<'a>),

        FunctionApplication(FunctionApplicationExpr<'a>),

        Parentised(ParentisedExpr<'a>),
        Block(BlockExpr<'a>),
        Ident(IdenExpr<'a>),
        If(IfExpr<'a>),
        While(WhileExpr<'a>),
        Literal(AnyLiteral<'a>),

        Machine(MachineTypeExpression<'a>),
    }

    impl<'a> NodeContainer<'a> for Expr<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                Expr::Infix(e) => e.container_visit(v),

                Expr::FunctionApplication(e) => e.container_visit(v),

                Expr::Parentised(e) => e.container_visit(v),
                Expr::Block(e) => e.container_visit(v),
                Expr::Ident(e) => e.container_visit(v),
                Expr::If(e) => e.container_visit(v),
                Expr::While(e) => e.container_visit(v),
                Expr::Literal(e) => e.container_visit(v),

                Expr::Machine(e) => e.container_visit(v),
            }
        }
    }

    impl<'a> Display for Expr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Parentised(e) => write!(f, "{e}")?,
                Self::Block(e) => write!(f, "{e}")?,
                Self::Ident(e) => write!(f, "{e}")?,
                Self::If(e) => write!(f, "{e}")?,
                Self::While(e) => write!(f, "{e}")?,
                Self::Literal(e) => write!(f, "{e}")?,

                Self::Infix(e) => write!(f, "({e})")?,
                Self::FunctionApplication(e) => write!(f, "({e})")?,
                Self::Machine(e) => write!(f, "({e})")?,
            }

            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum FileItem<'a> {
        Let(LetBinding<'a>),
    }

    impl<'a> NodeContainer<'a> for FileItem<'a> {
        fn container_visit<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            match self {
                FileItem::Let(l) => l.container_visit(v),
            }
        }
    }

    impl<'a> Display for FileItem<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                FileItem::Let(l) => write!(f, "{l}")?,
            }

            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct File<'a> {
        pub id: NodeId,

        pub declarations: Box<[FileItem<'a>]>,
        pub eof: EOFToken<'a>,
    }

    impl<'a> Node<'a> for File<'a> {
        fn id(&self) -> NodeId {
            self.id
        }

        fn walk<V: AstVisitor<'a>>(&'a self, v: &mut V) {
            for d in self.declarations.iter() {
                d.container_visit(v);
            }
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

    /// Creates a new (unique) [NodeId]
    // FIXME: Maybe make it not reliant on global variables
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

impl<'a, T: KnownToken<'a>> Parsable<'a> for Option<T> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        std::iter::once(T::KIND)
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Option<T>, ParserError> {
        Ok(ctx.tokens.get_eq()?)
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
        let mut items = Vec::new();
        let mut last = None;

        while ctx.tokens.peek_eq(End::KIND)?.is_none() {
            let item = T::parse(ctx)?;
            let coma = ctx.tokens.get_eq()?;

            if coma.is_none() || ctx.tokens.peek_eq(End::KIND)?.is_some() {
                last = Some((Box::new(item), coma));
                break;
            }

            items.push((
                item,
                coma.unwrap(),
            ));
        }
        ctx.tokens.peek_expected(End::KIND)?;

        Ok(Self {
            phantom: std::marker::PhantomData,
            items: items.into_boxed_slice(),
            last,
        })
    }
}

impl<'a> Parsable<'a> for MachineTypeExpression<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        [TokenType::Machine].into_iter()
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let machine_token = ctx.tokens.expected::<MachineToken>()?;
        let open = parse(ctx)?;
        let mut items = vec![];
        while ctx.tokens.peek_eq(TokenType::CurlyClose)?.is_none() {
            items.push(parse(ctx)?);
        }
        let close = parse(ctx)?;

        Ok(MachineTypeExpression {
            id: ctx.new_id(),

            machine_token,
            open,
            items: items.into_boxed_slice(),
            close,
        })
    }
}

impl<'a> Parsable<'a> for MachineItem<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            LetBinding,
            StateDeclaration
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Let => LetBinding,
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
        let iden = ctx.tokens.get_eq::<IdenToken>()?;

        let open = parse(ctx)?;
        let mut items = vec![];
        while ctx.tokens.peek_eq(TokenType::CurlyClose)?.is_none() {
            items.push(parse(ctx)?);
        }
        let close = parse(ctx)?;

        Ok(StateDeclaration {
            id: ctx.new_id(),

            initial_token,
            state_token,
            iden,
            open,
            items: items.into_boxed_slice(),
            close,
        })
    }
}

impl<'a> Parsable<'a> for StateItem<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            LetBinding,
            StateTransition,
            Data,
            Dyn
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        l_one!(ctx;
            Self::Let => LetBinding,
            Self::Transition => StateTransition,
            Self::Data => Data,
            Self::Dyn => Dyn
        )
    }
}

impl<'a> Parsable<'a> for StateTransition<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            FatArrowToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        Ok(StateTransition {
            id: ctx.new_id(),

            arrow: parse(ctx)?,
            name_id: parse(ctx)?,
            target_id: parse(ctx)?,
            semi: parse(ctx)?,
        })
    }
}

impl<'a> Parsable<'a> for LetBinding<'a> {
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
        let items = SeparatedList::parse(ctx)?;
        let curly_close = ctx.tokens.expected::<CurlyCloseToken>()?;

        Ok(Self {
            id: ctx.new_id(),

            data,
            curly_open,
            items,
            curly_close,
        })
    }
}

impl<'a> Parsable<'a> for DataItem<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            MutToken,
            IdenToken
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mutability = ctx.tokens.get_eq()?;
        let iden = parse(ctx)?;
        let ty = parse(ctx)?;

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
            Self::Ident => IdenExpr,
            Self::If => IfExpr,
            Self::While => WhileExpr,
            Self::Literal => AnyLiteral,
            Self::Machine => MachineTypeExpression
        )
    }

    const BOOLEAN_OR_PRECEDENCE: usize = 0;

    const BOOLEAN_AND_PRECEDENCE: usize = 1;

    const EQUALITY_PRECEDENCE: usize = 2;
    const LOWER_PRECEDENCE: usize = 2;
    const LOWER_EQUAL_PRECEDENCE: usize = 2;
    const GREATER_PRECEDENCE: usize = 2;
    const GREATER_EQUAL_PRECEDENCE: usize = 2;

    const PLUS_PRECEDENCE: usize = 3;
    const MINUS_PRECEDENCE: usize = 3;

    const TIMES_PRECEDENCE: usize = 4;
    const DIVIDE_PRECEDENCE: usize = 4;

    const FUNCTION_APPLICATION_PRECEDENCE: usize = 5;

    const BOTTOM_PRECEDENCE: usize = 6;

    fn expr_parse(
        ctx: &mut ParseContext<'a>, precedence: usize
    ) -> Result<Self, ParserError> {
        if precedence >= Self::BOTTOM_PRECEDENCE {
            return Self::bottom_parse(ctx);
        }

        let mut left = Self::expr_parse(ctx, precedence + 1)?;

        use TokenType as TT;

        macro_rules! take_infix {
            ($($tt:expr => [$p:expr],)*) => {
                match ctx.tokens.peek()?.content.as_ref() {
                $($tt if precedence == $p => {
                    left = Self::Infix(InfixOp {
                        id: ctx.new_id(),

                        left: Box::new(left),
                        op: ctx.tokens.expected()?,
                        right: Box::new(Expr::expr_parse(ctx, precedence + 1)?),
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
            take_infix!(
                "||" => [Self::BOOLEAN_OR_PRECEDENCE],

                "&&" => [Self::BOOLEAN_AND_PRECEDENCE],

                "==" => [Self::EQUALITY_PRECEDENCE],
                "<"  => [Self::LOWER_PRECEDENCE],
                "<=" => [Self::LOWER_EQUAL_PRECEDENCE],
                ">"  => [Self::GREATER_PRECEDENCE],
                ">=" => [Self::GREATER_EQUAL_PRECEDENCE],

                "+"  => [Self::PLUS_PRECEDENCE],
                "-"  => [Self::MINUS_PRECEDENCE],

                "*"  => [Self::TIMES_PRECEDENCE],
                "/"  => [Self::DIVIDE_PRECEDENCE],
            );

            // Function calls after this
            if precedence == Self::FUNCTION_APPLICATION_PRECEDENCE &&
               Expr::expects(&ctx.tokens.peek()?.kind)
            {
                left = Expr::FunctionApplication(FunctionApplicationExpr {
                    callee: Box::new(left),
                    argument: Box::new(Expr::expr_parse(ctx, precedence + 1)?),
                    id: ctx.new_id(),
                });

                continue;
            }

            break;
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
            AnyLiteral,
            MachineTypeExpression
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
        let callee = Expr::expr_parse(ctx, Expr::FUNCTION_APPLICATION_PRECEDENCE)?;
        let argument = Expr::expr_parse(ctx, Expr::FUNCTION_APPLICATION_PRECEDENCE)?;

        Ok(Self {
            id: ctx.new_id(),
            callee: Box::new(callee),
            argument: Box::new(argument),
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

        enum PreItem<'a> {
            Expr(Expr<'a>),
            Let(LetBinding<'a>),
        }

        let mut items = Vec::new();
        let mut last_expr = None;

        while ctx.tokens.peek_eq(TokenType::CurlyClose)?.is_none() {
            let item = l_one!(ctx;
                PreItem::Expr => Expr,
                PreItem::Let => LetBinding
            )?;

            match item {
                PreItem::Let(d) => {
                    items.push(BlockItem::Let(d));
                },
                PreItem::Expr(e)
                    if ctx.tokens.peek_eq(TokenType::SemiColon)?.is_some() =>
                {
                    items.push(BlockItem::Expr(SemiedExpr {
                        id: ctx.new_id(),

                        expr: Box::new(e),
                        semi: parse(ctx)?,
                    }));
                },
                // Expr with no semi colon = end of block
                PreItem::Expr(e) => {
                    last_expr = Some(Box::new(e));
                    break
                }
            }
        }

        let close = parse(ctx)?;

        Ok(BlockExpr {
            id: ctx.new_id(),

            open,
            items: items.into_boxed_slice(),
            last_expr,
            close,
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
            LetBinding
        )
    }

    fn parse(ctx: &mut ParseContext<'a>) -> Result<Self, ParserError> {
        let mut decls = vec![];

        while ctx.tokens.peek_eq(TokenType::EOF)?.is_none() {
            decls.push(FileItem::Let(parse(ctx)?));
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
        let mut ctx = ParseContext::from_src("<test>", "{ let a = 10; let c = true && false; let b = a + c; }");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "{let a = 10;let c = (true && false);let b = (a + c);}");

        Ok(())
    }

    #[test]
    fn expr_function_application() -> Result<(), Box<dyn Error>> {
        let mut ctx = ParseContext::from_src("<test>", "(this_is_a_function 5 (5 5 5) (10 + 10))");
        let expr = Expr::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "((((this_is_a_function 5) (((5 5) 5))) ((10 + 10))))");

        Ok(())
    }

    #[test]
    fn machine_curly_full() -> Result<(), Box<dyn Error>> {
        let src = r#"
        machine {
            initial state first {
                data {
                    name string,
                    id u32,
                    mut received i32
                }

                => increment second;

                reveived >= 10 -> {};
            }

            state second {
                => decrement first;
            }
        }
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = MachineTypeExpression::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "machine { initial state first { data {name string,id u32,mut received i32} => increment second; (reveived >= 10) -> {}; } state second { => decrement first; } }");

        Ok(())
    }

    #[test]
    fn machine_in_machine() -> Result<(), Box<dyn Error>> {
        let src = r#"
        machine {
            let inner = machine {

            };

            initial state first {
                let even_more_inner = machine {
                };

                data {}
            }
        }
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = MachineTypeExpression::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "machine { let inner = (machine { }); initial state first { let even_more_inner = (machine { }); data {} } }");

        Ok(())
    }

    #[test]
    fn let_alone() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let test = 5;
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = LetBinding::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "let test = 5;");

        Ok(())
    }

    #[test]
    fn let_alone_fn() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let plus a b = a + b;
        "#;
        let mut ctx = ParseContext::from_src("<test>", src);
        let expr = LetBinding::parse(&mut ctx)?;

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
        let expr = LetBinding::parse(&mut ctx)?;

        assert_eq!(format!("{expr}"), "let plus a b = {let c = (a + b);(a * c)};");

        Ok(())
    }
}
