
pub mod ast {
    use std::marker::PhantomData;

    use crate::lexer::*;

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

    pub struct SeparatedList<'a, End: KnownToken<'a>, Sep: KnownToken<'a>, T> {
        pub phantom: PhantomData<*const &'a End>,

        pub stmts: Box<[(T, Sep)]>,
        pub last: Option<(Box<T>, Option<Sep>)>,
    }

    pub struct Machine<'a> {
        pub machine_token: MachineToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, MachineToken<'a>, MachineStmt<'a>>,
    }

    pub enum MachineStmt<'a> {
        State(State<'a>),
    }

    pub struct State<'a> {
        pub initial_token: Option<InitialToken<'a>>,
        pub state_token: StateToken<'a>,
        pub id: IdenToken<'a>,
        pub stmts: StmtContainer<'a, StateToken<'a>, StateStmt<'a>>,
    }

    pub enum StateStmt<'a> {
        Transition(StateTransition<'a>),
        Dyn(Dyn<'a>),
        Data(Data<'a>),
        On(On<'a>),
    }

    pub struct StateTransition<'a> {
        pub equal: EqualToken<'a>,
        pub name_id: IdenToken<'a>,
        pub caret_close: CaretCloseToken<'a>,
        pub target_id: IdenToken<'a>,
        pub semi: SemiColonToken<'a>,
    }

    pub struct Dyn<'a> {
        pub src_expr: Expr<'a>,
        pub thin_arrow: ThinArrowToken<'a>,
        pub stmt: Expr<'a>,
    }

    pub struct Data<'a> {
        pub data: DataToken<'a>,
        pub curly_open: CurlyOpenToken<'a>,
        pub stmts: SeparatedList<'a, CurlyCloseToken<'a>, ComaToken<'a>, DataStmt<'a>>,
        pub curly_close: CurlyCloseToken<'a>,
    }

    pub struct DataStmt<'a> {
        pub mutability: Option<MutToken<'a>>,
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    pub struct On<'a> {
        pub on: OnToken<'a>,
        pub id: IdenToken<'a>,
        pub paren_open: ParenOpenToken<'a>,
        pub params: SeparatedList<'a, ParenCloseToken<'a>, ComaToken<'a>, Parameter<'a>>,
        pub paren_close: ParenCloseToken<'a>,
    }

    pub struct Parameter<'a> {
        pub id: IdenToken<'a>,
        pub ty: Type<'a>,
    }

    pub enum Type<'a> {
        Named(GenericToken<'a>),
    }

    pub struct BinOp<'a>{
        pub left: Box<Expr<'a>>,
        pub op: GenericToken<'a>,
        pub right: Box<Expr<'a>>,
    }

    pub struct ParentisedExpr<'a>{
        pub open: ParenOpenToken<'a>,
        pub expr: Box<Expr<'a>>,
        pub close: ParenCloseToken<'a>,
    }

    pub struct BlockExpr<'a> {
        pub open: CurlyOpenToken<'a>,
        pub exprs: SeparatedList<'a, CurlyCloseToken<'a>, SemiColonToken<'a>, Expr<'a>>,
        pub close: CurlyCloseToken<'a>,
    }

    pub struct IfExpr<'a>{
        pub if_: IfToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub then: Box<Expr<'a>>,
        pub else_: Option<Box<Expr<'a>>>,
    }

    pub struct WhileExpr<'a>{
        pub while_: WhileToken<'a>,
        pub cond: Box<Expr<'a>>,
        pub do_: Box<Expr<'a>>,
    }

    pub enum Expr<'a> {
        Parentised(ParentisedExpr<'a>),
        Block(BlockExpr<'a>),
        Id(GenericToken<'a>),
        BinOp(BinOp<'a>),
        If(IfExpr<'a>),
        While(WhileExpr<'a>),
    }
}
use std::{ops::Deref, marker::PhantomData};

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
            GenericToken { kind, .. } if $x::expected_first().any(|t| &t == kind) => {
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

trait ExprParse<'a, const PRES: usize>: Sized {
    fn expr_parse(
        tokens: &mut TokenStream<'a>,
    ) -> Result<Self, ParserError>;
}

impl<'a> Parsable<'a> for Expr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!()
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        ExprParse::<'a, 0>::expr_parse(tokens)
    }
}

impl<'a, const PRES: usize> ExprParse<'a, PRES> for Expr<'a> {
    fn expr_parse(
        tokens: &mut TokenStream<'a>,
    ) -> Result<Self, ParserError> {
        let left: Self = l_one!(tokens;
            Self::Parentised => ParentisedExpr {
                ParentisedExpr::expr_parse(tokens, ctx)
            },
            Self::Block => BlockExpr {
                BlockExpr::expr_parse(tokens, ctx)
            }
        )?;

        todo!()
    }
}

impl<'a> Parsable<'a> for ParentisedExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            ParenOpenToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let open = tokens.expected::<ParenOpenToken>()?;
        let expr = Box::new(Expr::parse(tokens)?);
        let close = tokens.expected::<ParenCloseToken>()?;
        Ok(ParentisedExpr {
            open,
            expr,
            close
        })
    }
}

impl<'a> ExprParse<'a> for ParentisedExpr<'a> {
    fn expr_parse(
        tokens: &mut TokenStream<'a>,
        ctx: ExprParseContext<'a>,
    ) -> Result<Self, ParserError> {
        todo!()
    }
}

impl<'a> Parsable<'a> for BlockExpr<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!(
            CurlyOpenToken
        )
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        let open = tokens.expected::<CurlyOpenToken>()?;
        let exprs = Parsable::parse(tokens);
        let close = tokens.expected::<CurlyCloseToken>()?;
        Ok(BlockExpr {
            open,
            exprs,
            close
        })
    }
}

impl<'a> ExprParse<'a> for BlockExpr<'a> {
    fn expr_parse(
        tokens: &mut TokenStream<'a>,
        ctx: ExprParseContext<'a>,
    ) -> Result<Self, ParserError> {
        todo!()
    }
}

impl<'a> Parsable<'a> for Type<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!()
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        todo!()
    }
}

impl<'a> Parsable<'a> for Parameter<'a> {
    fn expected_first() -> impl Iterator<Item = TokenType> + Clone {
        expected!()
    }

    fn parse(tokens: &mut TokenStream<'a>) -> Result<Self, ParserError> {
        todo!()
    }
}
