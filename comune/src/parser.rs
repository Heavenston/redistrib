
pub mod ast {
    use std::marker::PhantomData;

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

    /// List of nodes separated by a token, usually a comma (ex. arguments)
    #[derive(Debug)]
    pub struct SeparatedList<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T> {
        pub phantom: PhantomData<*const &'a End>,

        pub stmts: Box<[(T, Sep)]>,
        pub last: Option<(Box<T>, Option<Sep>)>,
    }

    /// A Machine statement
    #[derive(Debug)]
    pub struct Machine<'a> {
        pub machine_token: MachineToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, MachineToken<'a>, MachineStmt<'a>>,
    }

    /// Statements that go into a Machine
    #[derive(Debug, From)]
    pub enum MachineStmt<'a> {
        State(State<'a>),
    }

    /// A State statement
    #[derive(Debug)]
    pub struct State<'a> {
        pub initial_token: Option<InitialToken<'a>>,
        pub state_token: StateToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, StateToken<'a>, StateStmt<'a>>,
    }

    /// Statements that go into a State
    #[derive(Debug, From)]
    pub enum StateStmt<'a> {
        Transition(StateTransition<'a>),
        Dyn(Dyn<'a>),
        Data(Data<'a>),
        On(On<'a>),
    }

    /// A state-transition statement
    #[derive(Debug)]
    pub struct StateTransition<'a> {
        pub equal: EqualToken<'a>,
        pub name_id: IdenToken<'a>,
        pub caret_close: CaretCloseToken<'a>,
        pub target_id: IdenToken<'a>,
        pub semi: SemiColonToken<'a>,
    }

    /// A dynamic statement
    #[derive(Debug)]
    pub struct Dyn<'a> {
        pub src_expr: Expr0<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr0<'a>,
    }

    /// A state-data statement
    #[derive(Debug)]
    pub struct Data<'a> {
        pub data: DataToken<'a>,
        pub curly_open: CurlyOpenToken<'a>,
        pub stmts: SeparatedList<'a, CurlyCloseToken<'a>, ComaToken<'a>, DataStmt<'a>>,
        pub curly_close: CurlyCloseToken<'a>,
    }

    /// Statements that go into a Data statement
    #[derive(Debug)]
    pub struct DataStmt<'a> {
        pub mutability: Option<MutToken<'a>>,
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    /// A 'on' statement
    #[derive(Debug)]
    pub struct On<'a> {
        pub on: OnToken<'a>,
        pub id: IdenToken<'a>,
        pub paren_open: ParenOpenToken<'a>,
        pub params: SeparatedList<'a, ParenCloseToken<'a>, ComaToken<'a>, Parameter<'a>>,
        pub paren_close: ParenCloseToken<'a>,
    }

    /// A parameter of a function / 'on' statement
    #[derive(Debug)]
    pub struct Parameter<'a> {
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    /// A Type
    #[derive(Debug, From)]
    pub enum Type<'a> {
        Named(IdenToken<'a>),
    }

    /// Operator in the infix form
    #[derive(Debug)]
    pub struct InfixOp<'a, Left, Op: KnownToken<'a>, Right>{
        pub left: Box<Left>,
        pub op: Op,
        pub right: Box<Right>,
        pub p: PhantomData<*const &'a ()>,
    }

    /// An expression surrounded by parenthesis
    #[derive(Debug)]
    pub struct ParentisedExpr<'a>{
        pub open: ParenOpenToken<'a>,
        pub expr: Box<Expr0<'a>>,
        pub close: ParenCloseToken<'a>,
    }

    /// A list of expressions separated by semicolons and surrounded by 
    /// curly braces
    #[derive(Debug)]
    pub struct BlockExpr<'a> {
        pub open: CurlyOpenToken<'a>,
        pub exprs: SeparatedList<'a, CurlyCloseToken<'a>, SemiColonToken<'a>, Expr0<'a>>,
        pub close: CurlyCloseToken<'a>,
    }

    /// An if expression
    #[derive(Debug)]
    pub struct IfExpr<'a>{
        pub if_: IfToken<'a>,
        pub cond: Box<Expr0<'a>>,
        pub then: Box<Expr0<'a>>,
        pub else_: Option<(ElseToken<'a>, Box<Expr0<'a>>)>,
    }

    /// A while expression
    #[derive(Debug)]
    pub struct WhileExpr<'a>{
        pub while_: WhileToken<'a>,
        pub cond: Box<Expr0<'a>>,
        pub do_: Box<Expr0<'a>>,
    }

    /// A literal value
    #[derive(Debug, From)]
    pub enum AnyLiteral<'a> {
        Decimal(DecimalLiteral<'a>),
        String(StringLiteral<'a>),
    }

    /// A decimal literal
    #[derive(Debug, From)]
    pub struct DecimalLiteral<'a> {
        pub tok: DecimalLiteralToken<'a>,
    }

    /// A string literal
    #[derive(Debug, From)]
    pub struct StringLiteral<'a> {
        pub tok: StringLiteralToken<'a>,
    }

    /// An expression with highest precedence
    /// Has the BooleanOr binary operator
    #[derive(Debug, From)]
    pub enum Expr0<'a> {
        Expr(Expr1<'a>),
        BooleanOr(InfixOp<'a, Expr0<'a>, DoubleVBarToken<'a>, Expr1<'a>>),
    }

    impl<'a> From<Expr2<'a>> for Expr0<'a> {
        fn from(value: Expr2<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    impl<'a> From<Expr3<'a>> for Expr0<'a> {
        fn from(value: Expr3<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    impl<'a> From<Expr4<'a>> for Expr0<'a> {
        fn from(value: Expr4<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    /// An expression with intemediate precedance
    /// Has the BooleanAnd binary operator
    #[derive(Debug, From)]
    pub enum Expr1<'a> {
        Expr(Expr2<'a>),
        BooleanAnd(InfixOp<'a, Expr1<'a>, DoubleAndToken<'a>, Expr2<'a>>),
    }

    impl<'a> From<Expr3<'a>> for Expr1<'a> {
        fn from(value: Expr3<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    impl<'a> From<Expr4<'a>> for Expr1<'a> {
        fn from(value: Expr4<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    /// An expression with intemediate precedance
    /// Has the Plus and Minus binary operators
    #[derive(Debug, From)]
    pub enum Expr2<'a> {
        Expr(Expr3<'a>),
        Plus(InfixOp<'a, Expr2<'a>, PlusToken<'a>, Expr3<'a>>),
        Minus(InfixOp<'a, Expr2<'a>, DashToken<'a>, Expr3<'a>>),
    }

    impl<'a> From<Expr4<'a>> for Expr2<'a> {
        fn from(value: Expr4<'a>) -> Self {
            Self::Expr(value.into())
        }
    }

    /// An expression with lowest precedance
    /// Has the Times and Divide biary operators
    #[derive(Debug, From)]
    pub enum Expr3<'a> {
        Expr(Expr4<'a>),
        Times(InfixOp<'a, Expr3<'a>, StarToken<'a>, Expr4<'a>>),
        Divide(InfixOp<'a, Expr3<'a>, FSlashToken<'a>, Expr4<'a>>),
    }

    /// An expression with no binary operators
    /// Has the Times and Divide biary operators
    #[derive(Debug, From)]
    pub enum Expr4<'a> {
        Parentised(ParentisedExpr<'a>),
        Block(BlockExpr<'a>),
        Id(IdenToken<'a>),
        If(IfExpr<'a>),
        While(WhileExpr<'a>),
        Literal(AnyLiteral<'a>),
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

impl<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T: Parsable<'a>> Parsable<'a> for SeparatedList<'a, End, Sep, T> {
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

impl<'a> Parsable<'a> for Machine<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        [TokenType::Machine].into_iter()
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let machine_token = tokens.expected::<MachineToken>()?;
        let id = tokens.expected::<IdenToken>()?;
        let stmts = StmtContainer::parse(tokens)?;

        Ok(Machine {
            machine_token,
            id,
            stmts,
        })
    }
}

impl<'a> Parsable<'a> for MachineStmt<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            State
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
            Self::State => State
        )
    }
}

impl<'a> Parsable<'a> for State<'a> {
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

        Ok(State {
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
            StateTransition,
            Data,
            On,
            Dyn
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(
            tokens;

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
        let equal = tokens.expected::<EqualToken>()?;
        let name_id = tokens.expected::<IdenToken>()?;
        let caret_close = tokens.expected::<CaretCloseToken>()?;
        let target_id = tokens.expected::<IdenToken>()?;
        let semi = tokens.expected::<SemiColonToken>()?;

        Ok(StateTransition {
            equal,
            name_id,
            caret_close,
            target_id,
            semi
        })
    }
}

impl<'a> Parsable<'a> for Dyn<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr0
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let src_expr = Expr0::parse(tokens)?;
        let thin_arrow = tokens.expected::<ThinArrowToken>()?;
        let stmt = todo!();

        Ok(Self {
            src_expr,
            thin_arrow,
            stmt,
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

        Ok(Self {
            on,
            id,
            paren_open,
            params,
            paren_close,
        })
    }
}

impl<'a> Parsable<'a> for AnyLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            StringLiteral,
            DecimalLiteral
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
            Self::String => StringLiteral,
            Self::Decimal => DecimalLiteral
        )
    }
}

impl<'a> Parsable<'a> for DecimalLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            DecimalLiteralToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            tok: tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for StringLiteral<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            StringLiteralToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        Ok(Self {
            tok: tokens.expected()?,
        })
    }
}

impl<'a> Parsable<'a> for Expr0<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr1
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut left = Self::Expr(Expr1::parse(tokens)?);

        loop {
            if let Some(op) = tokens.get_eq()? {
                left = Self::BooleanOr(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else {
                break;
            }
        }
        return Ok(left);
    }
}

impl<'a> Parsable<'a> for Expr1<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr2
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut left = Self::Expr(Expr2::parse(tokens)?);

        loop {
            if let Some(op) = tokens.get_eq()? {
                left = Self::BooleanAnd(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else {
                break;
            }
        }
        return Ok(left);
    }
}

impl<'a> Parsable<'a> for Expr2<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            Expr3
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut left = Self::Expr(Expr3::parse(tokens)?);

        loop {
            if let Some(op) = tokens.get_eq()? {
                left = Self::Plus(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else if let Some(op) = tokens.get_eq()? {
                left = Self::Minus(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else {
                break;
            }
        }
        return Ok(left);
    }
}

impl<'a> Parsable<'a> for Expr3<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParentisedExpr,
            BlockExpr,
            IfExpr,
            WhileExpr,
            AnyLiteral
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let mut left = Self::Expr(Expr4::parse(tokens)?);

        loop {
            if let Some(op) = tokens.get_eq()? {
                left = Self::Times(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else if let Some(op) = tokens.get_eq()? {
                left = Self::Divide(InfixOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(Parsable::parse(tokens)?),
                    p: PhantomData,
                });
            }
            else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'a> Parsable<'a> for Expr4<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParentisedExpr,
            BlockExpr,
            IfExpr,
            WhileExpr,
            AnyLiteral
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        l_one!(tokens;
            Self::Parentised => ParentisedExpr,
            Self::Block => BlockExpr,
            Self::Id => IdenToken,
            Self::If => IfExpr,
            Self::While => WhileExpr,
            Self::Literal => AnyLiteral
        )
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
            open: Parsable::parse(tokens)?,
            expr: Box::new(Parsable::parse(tokens)?),
            close: Parsable::parse(tokens)?,
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
        Ok(BlockExpr {
            open: Parsable::parse(tokens)?,
            exprs: Parsable::parse(tokens)?,
            close: Parsable::parse(tokens)?,
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
            id: Parsable::parse(tokens)?,
            ty: Parsable::parse(tokens)?,
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

        let cond: Expr0;
        let then: Expr0;
        if tokens.peek_eq(TokenType::ParenOpen)?.is_some() {
            cond = Expr4::Parentised(ParentisedExpr::parse(tokens)?)
                .into();
            then = Expr0::parse(tokens)?;
        }
        else {
            cond = Expr0::parse(tokens)?;
            then = Expr4::Block(BlockExpr::parse(tokens)?)
                .into();
        }

        let mut else_ = None::<(ElseToken<'a>, Box<Expr0<'a>>)>;
        if tokens.peek_eq(TokenType::Else)?.is_some() {
            let tok = tokens.expected::<ElseToken>()?;
            let expr;
            if tokens.peek_eq(TokenType::If)?.is_some() {
                expr = Expr4::If(Parsable::parse(tokens)?);
            }
            else {
                expr = Expr4::Block(Parsable::parse(tokens)?);
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

        let cond: Expr0<'a>;
        let do_: Expr0<'a>;
        if tokens.peek_eq(TokenType::ParenOpen)?.is_some() {
            cond = Expr4::Parentised(ParentisedExpr::parse(tokens)?)
                .into();
            do_ = Expr0::parse(tokens)?;
        }
        else {
            cond = Expr0::parse(tokens)?;
            do_ = Expr4::Block(BlockExpr::parse(tokens)?)
                .into();
        }

        Ok(Self {
            while_,
            cond: Box::new(cond),
            do_: Box::new(do_),
        })
    }
}
