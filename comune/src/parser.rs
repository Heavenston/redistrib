
pub mod ast {
    use std::marker::PhantomData;
    use std::fmt::{ Debug, Display };

    use crate::lexer::*;

    use derive_more::*;

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
        pub let_: LetToken<'a>,
        pub mut_: Option<MutToken<'a>>,
        pub name: IdenToken<'a>,
        pub params: Box<[IdenToken<'a>]>,
        pub eq: EqualToken<'a>,
        pub expr: Expr<'a>,
        pub semi: SemiColonToken<'a>,
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
        pub machine_token: MachineToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, MachineToken<'a>, MachineStmt<'a>>,
    }

    impl<'a> Display for MachineDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ", self.machine_token)?;
            write!(f, "{} ", self.id)?;
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
        pub initial_token: Option<InitialToken<'a>>,
        pub state_token: StateToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, StateToken<'a>, StateStmt<'a>>,
    }

    impl<'a> Display for StateDeclaration<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(initial) = &self.initial_token {
                write!(f, "{} ", initial)?;
            }
            write!(f, "{} ", self.state_token)?;
            write!(f, "{} ", self.id)?;
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
        pub equal: EqualToken<'a>,
        pub name_id: IdenToken<'a>,
        pub caret_close: CaretCloseToken<'a>,
        pub target_id: IdenToken<'a>,
        pub semi: SemiColonToken<'a>,
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
        pub src_expr: Expr<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr<'a>,
        /// Only optional if stmt is a block statement
        pub semi: Option<SemiColonToken<'a>>,
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
        pub data: DataToken<'a>,
        pub curly_open: CurlyOpenToken<'a>,
        pub stmts: SeparatedList<'a, CurlyCloseToken<'a>, ComaToken<'a>, DataStmt<'a>>,
        pub curly_close: CurlyCloseToken<'a>,
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
        pub mutability: Option<MutToken<'a>>,
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    impl<'a> Display for DataStmt<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(mut_) = &self.mutability {
                write!(f, "{mut_} ")?;
            }
            write!(f, "{} {}",
                self.id, self.ty
            )?;

            Ok(())
        }
    }

    /// A 'on' statement
    #[derive(Debug)]
    pub struct On<'a> {
        pub on: OnToken<'a>,
        pub id: IdenToken<'a>,
        pub paren_open: ParenOpenToken<'a>,
        pub params: SeparatedList<'a, ParenCloseToken<'a>, ComaToken<'a>, Parameter<'a>>,
        pub paren_close: ParenCloseToken<'a>,
        pub body: BlockExpr<'a>,
    }

    impl<'a> Display for On<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}{}{} {}",
                self.on, self.id, self.paren_open, self.params,
                self.paren_close, self.body
            )?;

            Ok(())
        }
    }

    /// A parameter of a function / 'on' statement
    #[derive(Debug)]
    pub struct Parameter<'a> {
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    impl<'a> Display for Parameter<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {}",
                self.id, self.ty
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
        pub left: Box<Left>,
        pub op: Op,
        pub right: Box<Right>,
        pub p: PhantomData<*const &'a ()>,
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
        pub open: ParenOpenToken<'a>,
        pub expr: Box<Expr<'a>>,
        pub close: ParenCloseToken<'a>,
    }

    impl<'a> Display for ParentisedExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}{}", self.open, self.expr, self.close)?;

            Ok(())
        }
    }

    #[derive(Debug)]
    pub struct SemiedExpr<'a> {
        pub expr: Box<Expr<'a>>,
        pub semi: SemiColonToken<'a>,
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
        pub open: CurlyOpenToken<'a>,

        pub stmts: Box<[BlockStatement<'a>]>,
        /// If there is an expression without semi colon at the end
        /// it is placed here
        pub last_expr: Option<Box<Expr<'a>>>,

        pub close: CurlyCloseToken<'a>,
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
        pub if_: IfToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub then: Box<Expr<'a>>,
        pub else_: Option<(ElseToken<'a>, Box<Expr<'a>>)>,
    }

    impl<'a> Display for IfExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ", self.if_)?;
            write!(f, "{} ", self.cond)?;
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
        pub while_: WhileToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub do_: Box<Expr<'a>>,
    }

    impl<'a> Display for WhileExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.while_)?;
            write!(f, " {} ", self.cond)?;
            write!(f, "{}", self.do_)?;

            Ok(())
        }
    }

    /// A literal value
    #[derive(Debug, From)]
    pub enum AnyLiteral<'a> {
        Decimal(DecimalLiteralToken<'a>),
        String(StringLiteralToken<'a>),
        True(TrueToken<'a>),
        False(FalseToken<'a>),
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
    pub struct FunctionCallExpr<'a> {
        pub expr: Box<Expr<'a>>,
        pub paren_open: ParenOpenToken<'a>,
        pub arguments: SeparatedList<'a,
            ParenCloseToken<'a>, ComaToken<'a>, Expr<'a>
        >,
        pub paren_close: ParenCloseToken<'a>,
    }

    impl<'a> Display for FunctionCallExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}{}{}",
                self.expr, self.paren_open, self.arguments, self.paren_close
            )
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

        FunctionCall(FunctionCallExpr<'a>),

        Parentised(ParentisedExpr<'a>),
        Block(BlockExpr<'a>),
        Id(IdenToken<'a>),
        If(IfExpr<'a>),
        While(WhileExpr<'a>),
        Literal(AnyLiteral<'a>),
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
                Self::FunctionCall(e) => write!(f, "({e})")?,
            }

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
    (use $tokens:ident, $x:ident) => {
        $x::parse($tokens)
    };
    (use $tokens:ident, $x:ident $expr:block) => {
        $expr
    };
    (
        $tokens: ident;
        $($t:path => $x:ident $($exps:block)?),+
    ) => {
        match $tokens.peek_expecteds(
            [].into_iter()
            $(.chain(<$x>::expected_first()))+
        )? {
            $(
            GenericToken { kind, .. } if $x::expects(Some(kind)) => {
                l_one!(use $tokens, $x $($exps)?).map($t)
            }
            )+

            _ => unreachable!(),
        }
    };
}

macro_rules! match_token {
    (
        $tokens: ident;

        $($t:ident @ $x:ident => $b:block)+
    ) => {
        match $tokens.expecteds([
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
        let p = self.peek()?
            .and_then(|tok| 
                tys.clone().any(|ty| ty == tok.kind)
                .then_some(tok)
            )
            .and(Some(()))
            .and_then(|()| self.get().transpose()).transpose()?;
        match p {
            Some(p) => Ok(p),
            None => Err(ParserError::UnexpetedToken {
                read: self.peek()?.map(GenericToken::to_static),
                expected: tys.collect::<Vec<_>>().into_boxed_slice()
            })
        }
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
        match self.peek()? {
            Some(p) if tys.clone().any(|ty| ty == p.kind) => Ok(p),
            Some(p) => {
                Err(ParserError::UnexpetedToken {
                    read: Some(p.to_static()),
                    expected: tys.collect::<Vec<_>>().into_boxed_slice()
                })
            }
            None => {
                Err(ParserError::UnexpetedToken {
                    read: None,
                    expected: tys.collect::<Vec<_>>().into_boxed_slice()
                })
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Unexpected token, received {read:?} expected {expected:?}")]
    UnexpetedToken {
        read: Option<GenericToken<'static>>,
        expected: Box<[TokenType]>,
    },
    #[error(transparent)]
    LexerError(#[from] LexerError),
}

pub trait Parsable<'a>: Sized {
    fn expects(e: Option<&TokenType>) -> bool {
        Self::expected_first().any(|t| Some(&t) == e)
    }

    fn expected_first() -> impl Iterator<Item = TokenType> + Clone;

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError>;
}

pub fn parse<'a, T: Parsable<'a>>(
    tokens: &mut TokenStream<'a>
) -> Result<T, ParserError> {
    T::parse(tokens)
}

impl<'a, T: KnownToken<'a>> Parsable<'a> for T {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        std::iter::once(T::KIND)
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        tokens.expected()
    }
}

impl<'a, End: KnownToken<'a>, T: Parsable<'a>> Parsable<'a> for StmtContainer<'a, End, T> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            SemiColonToken,
            CurlyOpenToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut stmts = vec![];
        match_token!(tokens;
            t @ SemiColonToken => {
                while tokens.peek()?.is_some_and(|t| t.kind != End::KIND) {
                    stmts.push(T::parse(tokens)?);
                }

                Ok(Self::UseSemi { semi: t, stmts: stmts.into_boxed_slice() })
            }
            t @ CurlyOpenToken => {
                while tokens.peek()?.is_some_and(|t| t.kind != TokenType::CurlyClose) {
                    stmts.push(T::parse(tokens)?);
                }
                let close = tokens.expected::<CurlyCloseToken>()?;

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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut stmts = Vec::new();
        let mut last = None;

        while tokens.peek_eq(End::KIND)?.is_none() {
            let stmt = T::parse(tokens)?;
            let coma = tokens.get_eq()?;

            if coma.is_none() || tokens.peek_eq(End::KIND)?.is_some() {
                last = Some((Box::new(stmt), coma));
                break;
            }

            stmts.push((
                stmt,
                coma.unwrap(),
            ));
        }
        tokens.peek_expected(End::KIND)?;

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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let machine_token = tokens.expected::<MachineToken>()?;
        let id = tokens.expected::<IdenToken>()?;
        let stmts = StmtContainer::parse(tokens)?;

        Ok(MachineDeclaration {
            machine_token,
            id,
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let initial_token = tokens.get_eq::<InitialToken>()?;
        let state_token = tokens.expected::<StateToken>()?;
        let id = tokens.expected::<IdenToken>()?;
        let stmts = StmtContainer::parse(tokens)?;

        Ok(StateDeclaration {
            initial_token,
            state_token,
            id,
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;

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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(StateTransition {
            equal: parse(tokens)?,
            name_id: parse(tokens)?,
            caret_close: parse(tokens)?,
            target_id: parse(tokens)?,
            semi: parse(tokens)?,
        })
    }
}

impl<'a> Parsable<'a> for LetDeclaration<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            LetToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let let_ = parse(tokens)?;
        let mut_ = tokens.get_eq()?;
        let name = parse(tokens)?;
        let mut params = Vec::new();
        while tokens.peek_eq(TokenType::Equal)?.is_none() {
            params.push(parse(tokens)?);
        }
        let eq = parse(tokens)?;
        let expr = parse(tokens)?;
        let semi = parse(tokens)?;

        Ok(Self {
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let src_expr = Expr::parse(tokens)?;
        let thin_arrow = tokens.expected::<ThinArrowToken>()?;
        let stmt = Expr::parse(tokens)?;
        let is_block = matches!(stmt, Expr::Block(..));
        let semi = if is_block { tokens.get_eq()? } else { Some(tokens.expected()?) };

        Ok(Self {
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let data = tokens.expected::<DataToken>()?;
        let curly_open = tokens.expected::<CurlyOpenToken>()?;
        let stmts = SeparatedList::parse(tokens)?;
        let curly_close = tokens.expected::<CurlyCloseToken>()?;

        Ok(Self {
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mutability = tokens.get_eq::<MutToken>()?;
        let id = tokens.expected::<IdenToken>()?;
        let ty = Type::parse(tokens)?;

        Ok(Self {
            mutability,
            id,
            ty,
        })
    }
}

impl<'a> Parsable<'a> for On<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            OnToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let on = tokens.expected::<OnToken>()?;
        let id = tokens.expected::<IdenToken>()?;
        let paren_open = tokens.expected::<ParenOpenToken>()?;
        let params = SeparatedList::parse(tokens)?;
        let paren_close = tokens.expected::<ParenCloseToken>()?;
        let body = BlockExpr::parse(tokens)?;

        Ok(Self {
            on,
            id,
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
            StringLiteralToken,
            DecimalLiteralToken,
            TrueToken,
            FalseToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
            Self::String => StringLiteralToken,
            Self::Decimal => DecimalLiteralToken,
            Self::False => FalseToken,
            Self::True => TrueToken
        )
    }
}

impl<'a> Expr<'a> {
    fn bottom_parse(
        tokens: &mut TokenStream<'a>
    ) -> Result<Self, ParserError> {
        l_one!(tokens;
            Self::Parentised => ParentisedExpr,
            Self::Block => BlockExpr,
            Self::Id => IdenToken,
            Self::If => IfExpr,
            Self::While => WhileExpr,
            Self::Literal => AnyLiteral
        )
    }

    fn expr_parse(
        tokens: &mut TokenStream<'a>, precedence: usize
    ) -> Result<Self, ParserError> {
        if precedence >= 7 {
            return Self::bottom_parse(tokens);
        }

        let mut left = Self::expr_parse(tokens, precedence + 1)?;

        use TokenType as TT;

        macro_rules! take_precedence {
            ($($tt:path => $k:path [$p:expr],)*) => {
                match tokens.peek()?.map(|t| t.kind) {
                $(Some($tt) if precedence == $p => {
                    left = $k(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence + 1)?),
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
                TT::Equal           => Self::Assignment   [0],

                TT::DoubleVBar      => Self::BooleanOr    [1],

                TT::DoubleAnd       => Self::BooleanAnd   [2],

                TT::DoubleEqual     => Self::Equality     [3],
                TT::CaretOpen       => Self::Lower        [3],
                TT::CaretOpenEqual  => Self::LowerEqual   [3],
                TT::CaretClose      => Self::Greater      [3],
                TT::CaretCloseEqual => Self::GreaterEqual [3],

                TT::Plus            => Self::Plus         [4],
                TT::Dash            => Self::Minus        [4],

                TT::Star            => Self::Times        [5],
                TT::FSlash          => Self::Divide       [5],
            );

            match tokens.peek()?.map(|t| t.kind) {
                Some(TokenType::ParenOpen) if precedence == 6 => {
                    left = Self::FunctionCall(FunctionCallExpr {
                        expr: Box::new(left),
                        paren_open: tokens.expected()?,
                        arguments: parse(tokens)?,
                        paren_close: tokens.expected()?
                    });
                    continue;
                }

                _ => (),
            }

            // Only reachable if the peeked token does not match any
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
            IdenToken,
            IfExpr,
            WhileExpr,
            AnyLiteral
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Self::expr_parse(tokens, 0)
    }
}

impl<'a> Parsable<'a> for ParentisedExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParenOpenToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(ParentisedExpr {
            open: parse(tokens)?,
            expr: Box::new(parse(tokens)?),
            close: parse(tokens)?,
        })
    }
}

impl<'a> Parsable<'a> for FunctionCallExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            expr: Box::new(parse(tokens)?),
            paren_open: parse(tokens)?,
            arguments: parse(tokens)?,
            paren_close: parse(tokens)?,
        })
    }
}

impl<'a> Parsable<'a> for BlockExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            CurlyOpenToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let open = parse(tokens)?;

        enum ExprOrDecl<'a> {
            Expr(Expr<'a>),
            Decl(Declaration<'a>),
        }

        let mut stmts = Vec::new();
        let mut last_expr = None;

        while tokens.peek_eq(TokenType::CurlyClose)?.is_none() {
            let stmt = l_one!(tokens;
                ExprOrDecl::Expr => Expr,
                ExprOrDecl::Decl => Declaration
            )?;

            match stmt {
                ExprOrDecl::Decl(d) => {
                    stmts.push(BlockStatement::Decl(d));
                },
                ExprOrDecl::Expr(e)
                    if tokens.peek_eq(TokenType::SemiColon)?.is_some() =>
                {
                    stmts.push(BlockStatement::Expr(SemiedExpr {
                        expr: Box::new(e),
                        semi: parse(tokens)?,
                    }));
                },
                // Expr with no semi colon = end of block
                ExprOrDecl::Expr(e) => {
                    last_expr = Some(Box::new(e));
                    break
                }
            }
        }

        let close = parse(tokens)?;

        Ok(BlockExpr {
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            id: parse(tokens)?,
            ty: parse(tokens)?,
        })
    }
}

impl<'a> Parsable<'a> for IfExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            IfToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let if_ = tokens.expected::<IfToken>()?;

        let cond = Expr::parse(tokens)?;
        let then: Expr;
        if matches!(cond, Expr::Parentised(..)) {
            then = Expr::parse(tokens)?;
        }
        else {
            then = Expr::Block(BlockExpr::parse(tokens)?)
                .into();
        }

        let mut else_ = None::<(ElseToken<'a>, Box<Expr<'a>>)>;
        if tokens.peek_eq(TokenType::Else)?.is_some() {
            let tok = tokens.expected::<ElseToken>()?;
            let expr;
            if tokens.peek_eq(TokenType::If)?.is_some() {
                expr = Expr::If(parse(tokens)?);
            }
            else {
                expr = Expr::Block(parse(tokens)?);
            }
            else_ = Some((tok, Box::new(expr.into())));
        }

        Ok(Self {
            if_,
            cond: Box::new(cond),
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

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let while_ = tokens.expected()?;

        let cond: Expr<'a> = Expr::parse(tokens)?;
        let do_: Expr<'a>;
        if matches!(cond, Expr::Parentised(..)) {
            do_ = Expr::parse(tokens)?;
        }
        else {
            do_ = Expr::Block(BlockExpr::parse(tokens)?)
                .into();
        }

        Ok(Self {
            while_,
            cond: Box::new(cond),
            do_: Box::new(do_),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn expr_simple() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("5 + 5 / 5 + x * 5");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "((5 + (5 / 5)) + (x * 5))");

        Ok(())
    }

    #[test]
    fn expr_if() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if x == 10 { 5+5; }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if (x == 10) {(5 + 5);}");

        Ok(())
    }

    #[test]
    fn expr_if_else() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if (a + 5) == 10 { 5+5 } else { 0 }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if (((a + 5)) == 10) {(5 + 5)} else {0}");

        Ok(())
    }

    #[test]
    fn expr_if_else_if() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if (a + 5) == 10 { 5+5 } else if true { 1 } else { 0 }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if (((a + 5)) == 10) {(5 + 5)} else if true {1} else {0}");

        Ok(())
    }

    #[test]
    fn expr_while() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("while true { test }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "while true {test}");

        Ok(())
    }

    #[test]
    fn block() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("{ a = 10; c = true && false; b = a + c; }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "{(a = 10);(c = (true && false));(b = (a + c));}");

        Ok(())
    }

    #[test]
    fn block2() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("{ a = 10; c = true && false; b = a + c }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "{(a = 10);(c = (true && false));(b = (a + c))}");

        Ok(())
    }

    #[test]
    fn expr_function_call() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("(this_is_a_function + 5)(5, (10 + 10),)");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "(((this_is_a_function + 5))(5,((10 + 10)),))");

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
        let mut tokens = TokenStream::new(src);
        let expr = MachineDeclaration::parse(&mut tokens)?;

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
        let mut tokens = TokenStream::new(src);
        let expr = MachineDeclaration::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "machine name { machine inner { } initial state first { machine even_more_inner { } data {} } }");

        Ok(())
    }

    #[test]
    fn let_alone() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let test = 5;
        "#;
        let mut tokens = TokenStream::new(src);
        let expr = LetDeclaration::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "let test = 5;");

        Ok(())
    }

    #[test]
    fn let_alone_fn() -> Result<(), Box<dyn Error>> {
        let src = r#"
        let plus a b = a + b;
        "#;
        let mut tokens = TokenStream::new(src);
        let expr = LetDeclaration::parse(&mut tokens)?;

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
        let mut tokens = TokenStream::new(src);
        let expr = LetDeclaration::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "let plus a b = {let c = (a + b);(a * c)};");

        Ok(())
    }
}
