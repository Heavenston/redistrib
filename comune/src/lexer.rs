use std::{borrow::Cow, fmt::{Display, Debug}, marker::ConstParamTy};

#[derive(PartialEq, Debug, thiserror::Error)]
pub enum LexerError {
    #[error("Invalid character at {}:{}:{}", pos.file, pos.row, pos.col)]
    UnexpectedCharError {
        pos: SrcPose<'static>,
        char: Option<char>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SrcPose<'a> {
    pub file: Cow<'a, str>,
    pub row: u32,
    pub col: u32,
}

impl<'a> SrcPose<'a> {
    pub fn to_static(&self) -> SrcPose<'static> {
        SrcPose {
            file: self.file.clone().into_owned().into(),
            row: self.row,
            col: self.col,
        }
    }
}

impl<'a> PartialOrd for SrcPose<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.file != other.file {
            return None;
        }
        Some(match self.row.cmp(&other.row) {
            std::cmp::Ordering::Equal => self.col.cmp(&other.col),
            x => x,
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SrcRange<'a> {
    start_row: u32,
    start_col: u32,
    end_row: u32,
    end_col: u32,
    file: Cow<'a, str>,
}

impl<'a> SrcRange<'a> {
    pub fn to_static(&self) -> SrcRange<'static> {
        SrcRange {
            file: self.file.clone().into_owned().into(),
            start_row: self.start_row,
            start_col: self.start_col,
            end_row: self.end_row,
            end_col: self.end_col,
        }
    }

    pub fn start(&self) -> SrcPose<'a> {
        SrcPose {
            file: self.file.clone(),
            row: self.start_row,
            col: self.start_col,
        }
    }

    pub fn start_borrow(&self) -> SrcPose<'_> {
        SrcPose {
            file: Cow::Borrowed(&self.file),
            row: self.start_row,
            col: self.start_col,
        }
    }

    pub fn end(&self) -> SrcPose<'a> {
        SrcPose {
            file: self.file.clone(),
            row: self.end_row,
            col: self.end_col,
        }
    }

    pub fn end_borrow(&self) -> SrcPose<'_> {
        SrcPose {
            file: Cow::Borrowed(&self.file),
            row: self.end_row,
            col: self.end_col,
        }
    }

    pub fn from_start_end(start: SrcPose<'a>, end: &SrcPose<'a>) -> Self {
        assert_eq!(start.file, end.file, "Cannot range across files");
        Self {
            start_row: start.row,
            start_col: start.col,
            end_row: end.row,
            end_col: end.col,
            file: start.file,
        }
    }

    pub fn extend(&mut self, other: &Self) {
        assert_eq!(self.file, other.file, "Cannot extend across files");
        *self = Self::from_start_end(
            partial_min_max::min(other.start(), self.start()), 
            partial_min_max::max(&other.end(), &self.end()), 
        );
    }
}

pub trait Token<'a> {
    type Static: Token<'static> + 'static;

    fn kind(&self) -> TokenType;
    fn range(&self) -> &SrcRange<'a>;
    fn content(&'a self) -> &'a str;

    fn from_generic(gen: GenericToken<'a>) -> Option<Self>
        where Self: Sized;
    fn to_generic(&self) -> GenericToken<'a>;
    fn to_static(&self) -> Self::Static;
}

pub trait KnownToken<'a>: Token<'a> {
    const KIND: TokenType;
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GenericToken<'a> {
    pub kind: TokenType,
    pub content: Cow<'a, str>,
    pub range: SrcRange<'a>,
}

impl<'a> Debug for GenericToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token({:?}, kind {:?}, {}:{} -> {}:{})",
            self.kind,
            self.content.as_ref(),
            self.range.start().row, self.range.start().col,
            self.range.end().row, self.range.end().col,
        )
    }
}

impl<'a> Token<'a> for GenericToken<'a> {
    type Static = GenericToken<'static>;

    fn kind(&self) -> TokenType {
        self.kind
    }
    fn range(&self) -> &SrcRange<'a> {
        &self.range
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
            range: self.range.to_static()
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
                match self {
                    $(
                    Self::$name => write!(f, $print),
                    )*
                }
            }
        }

        $(
        #[derive(Clone, Hash, PartialEq, Eq)]
        pub struct $tname<'a> {
            pub content: Cow<'a, str>,
            pub range: SrcRange<'a>,
        }

        impl<'a> Display for $tname<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.content)
            }
        }

        impl<'a> Debug for $tname<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Token({:?}, {:?}({}:{} -> {}:{}))",
                    self.content.as_ref(),
                    self.range.start().file,
                    self.range.start().row,
                    self.range.start().col,
                    self.range.end().row,
                    self.range.end().col
                )
            }
        }

        impl<'a> Token<'a> for $tname<'a> {
            type Static = $tname<'static>;

            fn kind(&self) -> TokenType {
                TokenType::$name
            }
            fn range(&self) -> &SrcRange<'a> {
                &self.range
            }
            fn content(&'a self) -> &'a str {
                &*self.content
            }

            fn from_generic(gen: GenericToken<'a>) -> Option<Self> {
                if gen.kind == TokenType::$name {
                    Some(Self {
                        range: gen.range,
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
                    range: self.range.to_static(),
                }
            }
            fn to_generic(&self) -> GenericToken<'a> {
                GenericToken {
                    kind: TokenType::$name,
                    content: self.content.clone().into_owned().into(),
                    range: self.range.clone(),
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
    EOF(EOFToken, "<eof>"),

    Machine(MachineToken, "'machine'"),
    Initial(InitialToken, "'initial'"),
    State(StateToken, "'state'"),
    Data(DataToken, "'data'"),
    On(OnToken, "'on'"),
    Mut(MutToken, "'mut'"),
    Let(LetToken, "'let'"),

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
    CaretOpenEqual(CaretOpenEqualToken, "'<='"),
    CaretClose(CaretCloseToken, "'>'"),
    CaretCloseEqual(CaretCloseEqualToken, "'>='"),
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
    Equal(EqualToken, "'='"),
    DoubleEqual(DoubleEqualToken, "'=='")
);

const ID_CHARS_START: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
const ID_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$";

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Tokenizer<'a> {
    filename: &'a str,
    source: &'a str,
    peeking_index: usize,
    peeking_col: u32,
    peeking_row: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        Self {
            filename,
            source,
            peeking_index: 0,
            peeking_col: 0,
            peeking_row: 0,
        }
    }

    fn peeking_pos(&self) -> SrcPose<'a> {
        SrcPose {
            file: self.filename.into(),
            row: self.peeking_row,
            col: self.peeking_col,
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
        self.peeking_index += 1;
        if ch != '\n' {
            self.peeking_col += 1;
        }
        else {
            self.peeking_row += 1;
            self.peeking_col = 0;
        }

        Some(ch)
    }

    fn take_chars_while<F>(&mut self, mut p: F) -> &'a str
        where F: FnMut(char, &mut Self) -> bool
    {
        let start_index = self.peeking_index;
        let start_str = self.source;

        while self.peek_char().is_some_and(|c| p(c, self)) {
            self.take_char();
        }

        &start_str[0..self.peeking_index - start_index]
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
            "let" => Ok(TokenType::Let),

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
            // Peek because some branches need the char to not be taken
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

            Some('<') if self.take_char_if('=') => Ok(TokenType::CaretOpenEqual),
            Some('<') => Ok(TokenType::CaretOpen),

            Some('>') if self.take_char_if('=') => Ok(TokenType::CaretCloseEqual),
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
            Some('=') if self.take_char_if('=') => Ok(TokenType::DoubleEqual),
            Some('=') => Ok(TokenType::Equal),

            c => Err(LexerError::UnexpectedCharError {
                pos: self.peeking_pos().to_static(),
                char: c,
            }),
        }
    }

    pub fn get(&mut self) -> Result<GenericToken<'a>, LexerError> {
        while self.peek_char().is_some_and(|c| c.is_whitespace()) {
            self.take_char();
        }

        let ch = self.peek_char();

        let start_chs = self.peeking_index;
        let start_str = self.source;
        let start = self.peeking_pos();

        let kind = match ch {
            None => Ok(TokenType::EOF),
            Some('#') => Ok(self.read_comment()),
            Some('"') => self.read_string(),
            Some(c) if ID_CHARS_START.contains(c) => self.read_id(),
            Some(c) if c.is_digit(10) => self.read_decimal(),
            Some(_) => self.read_special(),
        };
        let kind = match kind {
            Ok(o) => o,
            Err(e) => return Err(e),
        };

        let len = self.peeking_index - start_chs;
        let content = Cow::from(&start_str[0..len]);

        Ok(GenericToken {
            kind,
            content,
            range: SrcRange::from_start_end(start, &self.peeking_pos()),
        })
    }
}

impl<'a> IntoIterator for Tokenizer<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;
    type IntoIter = TokenizerIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TokenizerIterator {
            izer: self,
            ended: false,
        }
    }
}

pub struct TokenizerIterator<'a> {
    izer: Tokenizer<'a>,
    ended: bool,
}

impl<'a> Iterator for TokenizerIterator<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.izer.get() {
            Ok(t) if t.kind == TokenType::EOF => {
                if self.ended
                { return None; }
                self.ended = true;
                Some(Ok(t))
            },
            e => Some(e)
        }
    }
}

pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    peeked: Vec<GenericToken<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(filename: &'a str, src: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(filename, src),
            peeked: Vec::new(),
        }
    }

    fn ignored_next(&mut self) -> Result<GenericToken<'a>, LexerError> {
        loop {
            match self.tokenizer.get() {
                Ok(t) if t.kind == TokenType::LineComment => (),
                Ok(t) if t.kind == TokenType::BlockComment => (),
                n => break n,
            }
        }
    }

    pub fn get(&mut self) -> Result<GenericToken<'a>, LexerError> {
        if self.peeked.len() > 0 {
            Ok(self.peeked.remove(0))
        }
        else {
            self.ignored_next()
        }
    }

    pub fn get_if<F>(
        &mut self, f: F
    ) -> Result<Option<GenericToken<'a>>, LexerError>
        where F: FnOnce(&GenericToken<'a>) -> bool
    {
        if f(self.peek()?) {
            return Ok(Some(self.get()?));
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

    pub fn peek_n(&mut self, n: usize) -> Result<&GenericToken<'a>, LexerError> {
        while self.peeked.len() <= n {
            let n = self.ignored_next()?;
            self.peeked.push(n);
        }

        Ok(&self.peeked[n])
    }

    pub fn peek(&mut self) -> Result<&GenericToken<'a>, LexerError> {
        self.peek_n(0)
    }

    pub fn peek_eq(
        &mut self, ty: TokenType
    ) -> Result<Option<&GenericToken<'a>>, LexerError> {
        let k = self.peek_n(0)?;
        if k.kind == ty {
            return Ok(Some(k));
        }
        Ok(None)
    }
}

impl<'a> IntoIterator for TokenStream<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;
    type IntoIter = TokenIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TokenIterator {
            stream: self,
            ended: false,
        }
    }
}

pub struct TokenIterator<'a> {
    stream: TokenStream<'a>,
    ended: bool,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<GenericToken<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.stream.get() {
            Ok(t) if t.kind == TokenType::EOF => {
                if self.ended
                { return None; }
                self.ended = true;
                Some(Ok(t))
            },
            e => Some(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    pub fn simple() {
        const SRC: &str = "machine -*+-\\ test-test
# Bite
data on state = 123456 + 031.4 -> 5
        ";

        for (i, t) in Tokenizer::new("<test>", SRC).into_iter().enumerate() {
            println!("{i:02} : {t:?}");
        }

        let mut tokens = Tokenizer::new("<test>", SRC);
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Machine,
            content: Cow::Borrowed("machine"),
            range: SrcRange {
                start_row: 0, start_col: 0,
                end_row: 0, end_col: 7,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Dash,
            content: Cow::Borrowed("-"),
            range: SrcRange {
                start_row: 0, start_col: 8,
                end_row: 0, end_col: 9,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Star,
            content: Cow::Borrowed("*"),
            range: SrcRange {
                start_row: 0, start_col: 9,
                end_row: 0, end_col: 10,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Plus,
            content: Cow::Borrowed("+"),
            range: SrcRange {
                start_row: 0, start_col: 10,
                end_row: 0, end_col: 11,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Dash,
            content: Cow::Borrowed("-"),
            range: SrcRange {
                start_row: 0, start_col: 11,
                end_row: 0, end_col: 12,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::BSlash,
            content: Cow::Borrowed("\\"),
            range: SrcRange {
                start_row: 0, start_col: 12,
                end_row: 0, end_col: 13,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Iden,
            content: Cow::Borrowed("test"),
            range: SrcRange {
                start_row: 0, start_col: 14,
                end_row: 0, end_col: 18,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Dash,
            content: Cow::Borrowed("-"),
            range: SrcRange {
                start_row: 0, start_col: 18,
                end_row: 0, end_col: 19,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Iden,
            content: Cow::Borrowed("test"),
            range: SrcRange {
                start_row: 0, start_col: 19,
                end_row: 0, end_col: 23,
                ..
            }
        }));

        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::LineComment,
            content: Cow::Borrowed("# Bite"),
            range: SrcRange {
                start_row: 1, start_col: 0,
                end_row: 1, end_col: 6,
                ..
            }
        }));

        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Data,
            content: Cow::Borrowed("data"),
            range: SrcRange {
                start_row: 2, start_col: 0,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::On,
            content: Cow::Borrowed("on"),
            range: SrcRange {
                start_row: 2, start_col: 5,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::State,
            content: Cow::Borrowed("state"),
            range: SrcRange {
                start_row: 2, start_col: 8,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Equal,
            content: Cow::Borrowed("="),
            range: SrcRange {
                start_row: 2, start_col: 14,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::DecimalLiteral,
            content: Cow::Borrowed("123456"),
            range: SrcRange {
                start_row: 2, start_col: 16,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::Plus,
            content: Cow::Borrowed("+"),
            range: SrcRange {
                start_row: 2, start_col: 23,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::DecimalLiteral,
            content: Cow::Borrowed("031.4"),
            range: SrcRange {
                start_row: 2, start_col: 25,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::ThinArrow,
            content: Cow::Borrowed("->"),
            range: SrcRange {
                start_row: 2, start_col: 31,
                ..
            }
        }));
        assert_matches!(tokens.get(), Ok(GenericToken {
            kind: TokenType::DecimalLiteral,
            content: Cow::Borrowed("5"),
            range: SrcRange {
                start_row: 2, start_col: 34,
                ..
            }
        }));
    }
}
