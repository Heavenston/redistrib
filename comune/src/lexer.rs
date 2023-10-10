use std::{borrow::Cow, fmt::Display, marker::ConstParamTy};

#[derive(PartialEq, Debug, thiserror::Error)]
pub enum LexerError {
    #[error("Invalid character at {row}:{col}")]
    UnexpectedCharError {
        row: u32,
        col: u32,
        char: Option<char>,
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TokenPosition {
    pub row: u32,
    pub col: u32,
}

pub trait Token<'a> {
    type Static: Token<'static> + 'static;

    fn kind(&self) -> TokenType;
    fn position(&self) -> TokenPosition;
    fn content(&'a self) -> &'a str;

    fn from_generic(gen: GenericToken<'a>) -> Option<Self>
        where Self: Sized;
    fn to_generic(&self) -> GenericToken<'a>;
    fn to_static(&self) -> Self::Static;
}

pub trait KnownToken<'a>: Token<'a> {
    const KIND: TokenType;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericToken<'a> {
    pub kind: TokenType,
    pub content: Cow<'a, str>,
    pub pos: TokenPosition,
}

impl<'a> Token<'a> for GenericToken<'a> {
    type Static = GenericToken<'static>;

    fn kind(&self) -> TokenType {
        self.kind
    }
    fn position(&self) -> TokenPosition {
        self.pos
    }
    fn content(&'a self) -> &'a str {
        self.content.as_ref()
    }

    fn from_generic(gen: GenericToken<'a>) -> Option<Self> {
        Some(gen)
    }
    fn to_generic(&self) -> GenericToken<'a> {
        self.clone()
    }
    fn to_static(&self) -> GenericToken<'static> {
        GenericToken::<'static> {
            kind: self.kind.clone(),
            content: self.content.clone().into_owned().into(),
            pos: self.pos,
        }
    }
}

macro_rules! tokens {
    (
        $($name:ident($tname:ident, $print:expr)),*
    ) => {
        #[derive(ConstParamTy, Clone, Copy, Debug, Hash, PartialEq, Eq)]
        pub enum TokenType {
            $($name),*
        }

        impl Display for TokenType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use TokenType as T;
                match self {
                    $(
                    T::$name => write!(f, $print),
                    )*
                }
            }
        }

        $(
        #[derive(Debug, Clone, Hash, PartialEq, Eq)]
        pub struct $tname<'a> {
            pub content: Cow<'a, str>,
            pub pos: TokenPosition,
        }

        impl<'a> Token<'a> for $tname<'a> {
            type Static = $tname<'static>;

            fn kind(&self) -> TokenType {
                TokenType::$name
            }
            fn position(&self) -> TokenPosition {
                self.pos
            }
            fn content(&'a self) -> &'a str {
                &*self.content
            }

            fn from_generic(gen: GenericToken<'a>) -> Option<Self> {
                if gen.kind == TokenType::$name {
                    Some(Self {
                        pos: gen.pos,
                        content: gen.content,
                    })
                }
                else {
                    None
                }
            }
            fn to_static(&self) -> $tname<'static> {
                $tname {
                    content: self.content.clone().into_owned().into(),
                    pos: self.pos,
                }
            }
            fn to_generic(&self) -> GenericToken<'a> {
                GenericToken {
                    kind: TokenType::$name,
                    content: self.content.clone().into_owned().into(),
                    pos: self.pos,
                }
            }
        }

        impl<'a> KnownToken<'a> for $tname<'a> {
            const KIND: TokenType = TokenType::$name;
        }
        )*
    };
}

tokens!(
    Machine(MachineToken, "'machine'"),
    Initial(InitialToken, "'initial'"),
    State(StateToken, "'state'"),
    Data(DataToken, "'data'"),
    On(OnToken, "'on'"),
    Mut(MutToken, "'mut'"),

    Iden(IdenToken, "<identifier>"),

    If(IfToken, "'if'"),
    Else(ElseToken, "'else'"),
    While(WhileToken, "'while'"),

    True(TrueToken, "'true'"),
    False(FalseToken, "'false'"),
    DecimalLiteral(DecimalLiteralToken, "<number>"),
    StringLiteral(StringLiteralToken, "<string>"),

    LineComment(LineCommentToken, "<comment>"),
    BlockComment(BlockCommentToken, "<block_comment>"),

    ThinArrow(ThinArrowToken, "'->'"),
    FatArrow(FatArrowToken, "'=>'"),

    VBar(VBarToken, "'|'"),
    DoubleVBar(DoubleVBarToken, "'||'"),
    FSlash(FSlashToken, "'/'"),
    BSlash(BSlashToken, "'\\'"),
    CurlyOpen(CurlyOpenToken, "'{{'"),
    CurlyClose(CurlyCloseToken, "'}}'"),
    ParenOpen(ParenOpenToken, "'('"),
    ParenClose(ParenCloseToken, "')'"),
    BracketOpen(BracketOpenToken, "'['"),
    BracketClose(BracketCloseToken, "']'"),
    CaretOpen(CaretOpenToken, "'<'"),
    CaretClose(CaretCloseToken, "'>'"),
    Colon(ColonToken, "':'"),
    Coma(ComaToken, "','"),
    SemiColon(SemiColonToken, "';'"),
    Quote(QuoteToken, "'"),
    DoubleQuote(DoubleQuoteToken, "'\"'"),
    And(AndToken, "'&'"),
    DoubleAnd(DoubleAndToken, "'&&'"),
    Bang(BangToken, "'!'"),
    QuestionMark(QuestionMarkToken, "'?'"),
    Dot(DotToken, "'.'"),
    Plus(PlusToken, "'+'"),
    Dash(DashToken, "'-'"),
    Star(StarToken, "'*'"),
    Equal(EqualToken, "'='")
);

const ID_CHARS_START: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
const ID_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$";

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Tokenizer<'a> {
    source: &'a str,
    chs: usize,
    col: u32,
    row: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            source: src,
            chs: 0,
            col: 0,
            row: 0,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.source.chars().next()
    }

    fn peek_char_n(&mut self, n: usize) -> Option<char> {
        self.source.chars().nth(n)
    }

    fn take_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;

        self.source = &self.source[1..];
        self.chs += 1;
        if ch != '\n' {
            self.col += 1;
        }
        else {
            self.row += 1;
            self.col = 0;
        }

        Some(ch)
    }

    fn take_chars_while<F>(&mut self, mut p: F) -> &'a str
        where F: FnMut(char, &mut Self) -> bool
    {
        let start_chs = self.chs;
        let start_str = self.source;

        while self.peek_char().is_some_and(|c| p(c, self)) {
            self.take_char();
        }

        &start_str[0..self.chs - start_chs]
    }

    fn take_char_if(&mut self, ch: char) -> bool {
        if self.peek_char() != Some(ch) {
            return false;
        }
        self.take_char();
        true
    }

    fn read_id(&mut self) -> Result<TokenType, LexerError> {
        debug_assert!(ID_CHARS_START.contains(
            self.peek_char().unwrap_or('\0')
        ));

        match self.take_chars_while(|c, _| ID_CHARS.contains(c)) {
            "machine" => Ok(TokenType::Machine),
            "initial" => Ok(TokenType::Initial),
            "state" => Ok(TokenType::State),
            "data" => Ok(TokenType::Data),
            "on" => Ok(TokenType::On),
            "mut" => Ok(TokenType::Mut),

            "if" => Ok(TokenType::If),
            "else" => Ok(TokenType::Else),
            "while" => Ok(TokenType::While),

            "true" => Ok(TokenType::True),
            "false" => Ok(TokenType::False),

            _ => Ok(TokenType::Iden),
        }
    }

    fn read_decimal(&mut self) -> Result<TokenType, LexerError> {
        self.take_chars_while(|c, this|
            c.is_digit(10) || (
                c == '.' &&
                this.peek_char_n(1).is_some_and(|c| c.is_digit(10))
            )
        );
        Ok(TokenType::DecimalLiteral)
    }

    fn read_escape(&mut self) -> Option<char> {
        assert_eq!(self.take_char(), Some('\\'));
        let new = self.take_char()?;
        Some(match new {
            '0' => '\0',
            'n' => '\n',
            _ => new,
        })
    }

    fn read_string(&mut self) -> Result<TokenType, LexerError> {
        assert_eq!(self.take_char(), Some('"'));

        loop {
            // Some branches need the char to not be taken
            match self.peek_char() {
                None => {
                    self.take_char();
                    break;
                },
                Some('"') => {
                    self.take_char();
                    break;
                },
                Some('\\') => match self.read_escape() {
                    None => break,
                    _ => (),
                }
                Some(_) => {
                    self.take_char();
                },
            }
        }

        Ok(TokenType::StringLiteral)
    }

    fn read_comment(&mut self) -> TokenType {
        assert_eq!(self.take_char(), Some('#'));
        if self.peek_char() == Some('[') {
            self.take_chars_while(|c, this| !(
                c == ']' && this.peek_char_n(1) == Some('#')
            ));
            assert_eq!(self.take_char(), Some(']'));
            assert_eq!(self.take_char(), Some('#'));

            TokenType::BlockComment
        }
        else {
            self.take_chars_while(|c, _| c != '\n');
            TokenType::LineComment
        }
    }
    
    fn read_special(&mut self) -> Result<TokenType, LexerError> {
        match self.take_char() {
            Some('{') => Ok(TokenType::CurlyOpen),
            Some('}') => Ok(TokenType::CurlyClose),
            Some('(') => Ok(TokenType::ParenOpen),
            Some(')') => Ok(TokenType::ParenClose),
            Some('[') => Ok(TokenType::BracketOpen),
            Some(']') => Ok(TokenType::BracketClose),
            Some('<') => Ok(TokenType::CaretOpen),
            Some('>') => Ok(TokenType::CaretClose),
            Some('/') => Ok(TokenType::FSlash),
            Some('\\') => Ok(TokenType::BSlash),
            Some('!') => Ok(TokenType::Bang),
            Some(':') => Ok(TokenType::Colon),
            Some(';') => Ok(TokenType::SemiColon),
            Some(',') => Ok(TokenType::Coma),
            Some('.') => Ok(TokenType::Dot),

            Some('|') if self.take_char_if('|') => Ok(TokenType::DoubleVBar),
            Some('|') => Ok(TokenType::VBar),

            Some('"') => Ok(TokenType::DoubleQuote),
            Some('\'') => Ok(TokenType::Quote),

            Some('&') if self.take_char_if('&') => Ok(TokenType::DoubleAnd),
            Some('&') => Ok(TokenType::And),

            Some('?') => Ok(TokenType::QuestionMark),
            Some('+') => Ok(TokenType::Plus),
            Some('-') if self.take_char_if('>') => Ok(TokenType::ThinArrow),
            Some('-') => Ok(TokenType::Dash),
            Some('*') => Ok(TokenType::Star),
            Some('=') if self.take_char_if('>') => Ok(TokenType::FatArrow),
            Some('=') => Ok(TokenType::Equal),

            c => Err(LexerError::UnexpectedCharError {
                row: self.row,
                col: self.col,
                char: c,
            }),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.peek_char()?.is_whitespace() {
            self.take_char();
        }

        let ch = self.peek_char()?;

        let start_chs = self.chs;
        let start_str = self.source;
        let row = self.row;
        let col = self.col;

        let kind = match ch {
            '#' => Ok(self.read_comment()),
            '"' => self.read_string(),
            _ if ID_CHARS_START.contains(ch) => self.read_id(),
            _ if ch.is_digit(10) => self.read_decimal(),
            _ => self.read_special(),
        };
        let kind = match kind {
            Ok(o) => o,
            Err(e) => return Some(Err(e)),
        };

        let len = self.chs - start_chs;
        let content = Cow::from(&start_str[0..len]);

        Some(Ok(GenericToken {
            kind,
            content,
            pos: TokenPosition { row: self.row, col: self.col }
        }))
    }
}

pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    peeked: Vec<GenericToken<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(src),
            peeked: Vec::new(),
        }
    }

    fn ignored_next(&mut self) -> Result<Option<GenericToken<'a>>, LexerError> {
        loop {
            match self.tokenizer.next() {
                Some(Ok(t)) if t.kind == TokenType::LineComment => (),
                Some(Ok(t)) if t.kind == TokenType::BlockComment => (),
                n => break n.transpose(),
            }
        }
    }

    pub fn get(&mut self) -> Result<Option<GenericToken<'a>>, LexerError> {
        self.next().transpose()
    }

    pub fn get_if<F>(&mut self, f: F) -> Result<Option<GenericToken<'a>>, LexerError>
        where F: FnOnce(&GenericToken<'a>) -> bool
    {
        if self.peek()?.is_some_and(f) {
            return Ok(self.get()?);
        }
        Ok(None)
    }

    pub fn get_eq<T: KnownToken<'a>>(
        &mut self
    ) -> Result<Option<T>, LexerError> {
        Ok(
            self.get_if(|t| t.kind == T::KIND)?
            .and_then(|t| T::from_generic(t))
        )
    }

    pub fn peek_n(&mut self, n: usize) -> Result<Option<&GenericToken<'a>>, LexerError> {
        while self.peeked.len() <= n {
            let t = match self.ignored_next() {
                Ok(Some(t)) => t,
                Ok(None) => return Ok(None),
                Err(e) => return Err(e),
            };
            self.peeked.push(t);
        }

        Ok(Some(&self.peeked[n]))
    }

    pub fn peek(&mut self) -> Result<Option<&GenericToken<'a>>, LexerError> {
        self.peek_n(0)
    }

    pub fn peek_eq(&mut self, ty: TokenType) -> Result<Option<&GenericToken<'a>>, LexerError> {
        match self.peek_n(0)? {
            Some(s) if s.kind == ty => Ok(Some(s)),
            _ => Ok(None),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.len() > 0 {
            Some(Ok(self.peeked.remove(0)))
        }
        else {
            self.ignored_next().transpose()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn simple() {
        const SRC: &str = "machine -*+-\\ test-test
# Bite
data on state = 123456 + 031.4
        ";

        for t in Tokenizer::new(SRC) {
            println!("{t:?}");
        }

        let mut tokens = Tokenizer::new(SRC);
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Machine,
            content: "machine".into(),
            pos: TokenPosition { row: 0, col: 0 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Dash,
            content: "-".into(),
            pos: TokenPosition { row: 0, col: 8 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Star,
            content: "*".into(),
            pos: TokenPosition { row: 0, col: 9 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Plus,
            content: "+".into(),
            pos: TokenPosition { row: 0, col: 10 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Dash,
            content: "-".into(),
            pos: TokenPosition { row: 0, col: 11 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::BSlash,
            content: "\\".into(),
            pos: TokenPosition { row: 0, col: 12 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Iden,
            content: "test".into(),
            pos: TokenPosition { row: 0, col: 14 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Dash,
            content: "-".into(),
            pos: TokenPosition { row: 0, col: 18 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Iden,
            content: "test".into(),
            pos: TokenPosition { row: 0, col: 19 },
        })));

        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::LineComment,
            content: "# Bite".into(),
            pos: TokenPosition { row: 1, col: 0 },
        })));

        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Data,
            content: "data".into(),
            pos: TokenPosition { row: 2, col: 0 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::On,
            content: "on".into(),
            pos: TokenPosition { row: 2, col: 5 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::State,
            content: "state".into(),
            pos: TokenPosition { row: 2, col: 8 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Equal,
            content: "=".into(),
            pos: TokenPosition { row: 2, col: 14 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::DecimalLiteral,
            content: "123456".into(),
            pos: TokenPosition { row: 2, col: 16 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::Plus,
            content: "+".into(),
            pos: TokenPosition { row: 2, col: 23 },
        })));
        assert_eq!(tokens.next(), Some(Ok(GenericToken {
            kind: TokenType::DecimalLiteral,
            content: "031.4".into(),
            pos: TokenPosition { row: 2, col: 25 },
        })));
    }
}
