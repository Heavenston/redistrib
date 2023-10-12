
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

    /// List of nodes separated by a token, usually a comma (ex. arguments)
    #[derive(Debug)]
    pub struct SeparatedList<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T> {
        pub phantom: PhantomData<*const &'a End>,

        pub stmts: Box<[(T, Sep)]>,
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
        pub src_expr: Expr<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr<'a>,
        pub semi: Option<SemiColonToken<'a>>,
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
    pub struct ParentisedExpr<'a>{
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

    /// A list of expressions separated by semicolons and surrounded by 
    /// curly braces
    #[derive(Debug)]
    pub struct BlockExpr<'a> {
        pub open: CurlyOpenToken<'a>,
        pub exprs: SeparatedList<'a, CurlyCloseToken<'a>, SemiColonToken<'a>, Expr<'a>>,
        pub close: CurlyCloseToken<'a>,
    }

    impl<'a> Display for BlockExpr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}{}", self.open, self.exprs, self.close)?;

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

    /// An expression with intemediate precedance
    /// Has the BooleanOr binary operator
    #[derive(Debug, From)]
    pub enum Expr<'a> {
        Assignment(InfixOp<'a, Expr<'a>, EqualToken<'a>, Expr<'a>>),

        BooleanOr(InfixOp<'a, Expr<'a>, DoubleVBarToken<'a>, Expr<'a>>),

        BooleanAnd(InfixOp<'a, Expr<'a>, DoubleAndToken<'a>, Expr<'a>>),

        Equality(InfixOp<'a, Expr<'a>, DoubleEqualToken<'a>, Expr<'a>>),

        Plus(InfixOp<'a, Expr<'a>, PlusToken<'a>, Expr<'a>>),
        Minus(InfixOp<'a, Expr<'a>, DashToken<'a>, Expr<'a>>),

        Times(InfixOp<'a, Expr<'a>, StarToken<'a>, Expr<'a>>),
        Divide(InfixOp<'a, Expr<'a>, FSlashToken<'a>, Expr<'a>>),

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
                Self::Equality(e) => write!(f, "{e}")?,
                Self::Plus(e) => write!(f, "({e})")?,
                Self::Minus(e) => write!(f, "({e})")?,
                Self::Times(e) => write!(f, "({e})")?,
                Self::Divide(e) => write!(f, "({e})")?,
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
            Expr
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let src_expr = Expr::parse(tokens)?;
        let thin_arrow = tokens.expected::<ThinArrowToken>()?;
        let stmt = Expr::parse(tokens)?;
        let is_block = matches!(stmt, Expr::Block(..));
        let semi = if is_block { None } else { Some(tokens.expected()?) };

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
        if precedence >= 6 {
            return Self::bottom_parse(tokens);
        }

        let mut left = Self::expr_parse(tokens, precedence + 1)?;

        loop {
            match tokens.peek()?.map(|t| t.kind) {
                Some(TokenType::Equal) if precedence == 0 => {
                    left = Self::Assignment(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })
                }

                Some(TokenType::DoubleVBar) if precedence == 1 => {
                    left = Self::BooleanOr(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })
                }

                Some(TokenType::DoubleAnd) if precedence == 2 => {
                    left = Self::BooleanAnd(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })
                }

                Some(TokenType::DoubleEqual) if precedence == 3 => {
                    left = Self::Equality(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })
                }

                Some(TokenType::Plus) if precedence == 4 => {
                    left = Self::Plus(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })
                }
                Some(TokenType::Dash) if precedence == 4 => {
                    left = Self::Minus(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })        
                }

                Some(TokenType::Star) if precedence == 5 => {
                    left = Self::Times(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })                
                }
                Some(TokenType::FSlash) if precedence == 5 => {
                    left = Self::Divide(InfixOp {
                        left: Box::new(left),
                        op: tokens.expected()?,
                        right: Box::new(Expr::expr_parse(tokens, precedence)?),
                        p: PhantomData,
                    })                        
                }

                _ => break,
            }
        }
        return Ok(left);
    }
}

impl<'a> Parsable<'a> for Expr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!()
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
                expr = Expr::If(Parsable::parse(tokens)?);
            }
            else {
                expr = Expr::Block(Parsable::parse(tokens)?);
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

        assert_eq!(format!("{expr}"), "(5 + (5 / 5)) + (x * 5)");

        Ok(())
    }

    #[test]
    fn expr_if() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if x == 10 { 5+5; }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if x == 10 {(5 + 5);}");

        Ok(())
    }

    #[test]
    fn expr_if_else() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if (a + 5) == 10 { 5+5 } else { 0 }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if ((a + 5)) == 10 {(5 + 5)} else {0}");

        Ok(())
    }

    #[test]
    fn expr_if_else_if() -> Result<(), Box<dyn Error>> {
        let mut tokens = TokenStream::new("if (a + 5) == 10 { 5+5 } else if true { 1 } else { 0 }");
        let expr = Expr::parse(&mut tokens)?;

        assert_eq!(format!("{expr}"), "if ((a + 5)) == 10 {(5 + 5)} else if true {1} else {0}");

        Ok(())
    }
}
