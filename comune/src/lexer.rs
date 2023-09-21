use std::{borrow::Cow, sync::Arc};

#[derive(PartialEq, Debug, thiserror::Error)]
pub enum LexerError {
    #[error("Invalid character at {row}:{col}")]
    UnexpectedCharError {
        row: u32,
        col: u32,
        char: Option<char>,
    },
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum TokenType {
    Machine,
    Initial,
    State,
    Data,
    On,

    Iden,

    If,
    Else,
    While,

    True,
    False,
    DecimalLiteral,
    StringLiteral(Arc<str>),

    LineComment,
    BlockComment,

    ThinArrow,
    FatArrow,

    VBar,
    DoubleVBar,
    FSlash,
    BSlash,
    CurlyOpen,
    CurlyClose,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    Colon,
    SemiColon,
    Quote,
    DoubleQuote,
    And,
    DoubleAnd,
    Bang,
    QuestionMark,
    Dot,
    Plus,
    Dash,
    Star,
    Equal,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub content: Cow<'a, str>,
    pub row: u32,
    pub col: u32,
}

const ID_CHARS_START: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
const ID_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$";

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Tokens<'a> {
    source: &'a str,
    chs: usize,
    col: u32,
    row: u32,
}

impl<'a> Tokens<'a> {
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

        let mut s = String::new();
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
                    Some(c) => s.push(c),
                    None => break,
                }
                Some(c) => {
                    self.take_char();
                    s.push(c);
                },
            }
        }

        Ok(TokenType::StringLiteral(s.into()))
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
            Some('/') => Ok(TokenType::FSlash),
            Some('\\') => Ok(TokenType::BSlash),
            Some('!') => Ok(TokenType::Bang),
            Some(',') => Ok(TokenType::Colon),
            Some(';') => Ok(TokenType::SemiColon),
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

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<'a>, LexerError>;

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

        Some(Ok(Token {
            kind,
            content,
            row,
            col,
        }))
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

        for t in Tokens::new(SRC) {
            println!("{t:?}");
        }

        let mut tokens = Tokens::new(SRC);
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Machine,
            content: "machine".into(),
            row: 0,
            col: 0
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Dash,
            content: "-".into(),
            row: 0,
            col: 8
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Star,
            content: "*".into(),
            row: 0,
            col: 9
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Plus,
            content: "+".into(),
            row: 0,
            col: 10
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Dash,
            content: "-".into(),
            row: 0,
            col: 11
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::BSlash,
            content: "\\".into(),
            row: 0,
            col: 12
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Iden,
            content: "test".into(),
            row: 0,
            col: 14
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Dash,
            content: "-".into(),
            row: 0,
            col: 18
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Iden,
            content: "test".into(),
            row: 0,
            col: 19
        })));

        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::LineComment,
            content: "# Bite".into(),
            row: 1,
            col: 0
        })));

        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Data,
            content: "data".into(),
            row: 2,
            col: 0
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::On,
            content: "on".into(),
            row: 2,
            col: 5
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::State,
            content: "state".into(),
            row: 2,
            col: 8
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Equal,
            content: "=".into(),
            row: 2,
            col: 14
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::DecimalLiteral,
            content: "123456".into(),
            row: 2,
            col: 16
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::Plus,
            content: "+".into(),
            row: 2,
            col: 23
        })));
        assert_eq!(tokens.next(), Some(Ok(Token {
            kind: TokenType::DecimalLiteral,
            content: "031.4".into(),
            row: 2,
            col: 25
        })));
    }
}
