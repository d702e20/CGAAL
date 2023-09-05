use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};

#[derive(Clone, Eq, PartialEq)]
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Lexer { input, pos: 0 }
    }

    fn skip_ws(&mut self) {
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
    }

    #[inline]
    fn peek(&self, offset: usize) -> Option<u8> {
        if self.pos + offset < self.input.len() {
            Some(self.input[self.pos + offset])
        } else {
            None
        }
    }

    #[inline]
    fn token(&mut self, len: usize, token: TokenKind) -> Token {
        let span = Span::new(self.pos, self.pos + len);
        self.pos += len;
        Token::new(token, span)
    }

    fn lex_alpha(&mut self) -> Token {
        let mut len = 1;
        while self
            .peek(len)
            .map_or(false, |c| c.is_ascii_alphanumeric() || c == b'_')
        {
            len += 1;
        }
        let word = std::str::from_utf8(&self.input[self.pos..self.pos + len]).unwrap();
        let kind = match word {
            "const" => TokenKind::KwConst,
            "label" => TokenKind::KwLabel,
            "player" => TokenKind::KwPlayer,
            "template" => TokenKind::KwTemplate,
            "endtemplate" => TokenKind::KwEndTemplate,
            "init" => TokenKind::KwInit,
            "min" => TokenKind::KwMin,
            "max" => TokenKind::KwMax,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Word(word.to_string()),
        };
        self.token(len, kind)
    }

    fn lex_num(&mut self) -> Token {
        let mut len = 1;
        while self.peek(len).map_or(false, |c| c.is_ascii_digit()) {
            len += 1;
        }
        let val: i32 = std::str::from_utf8(&self.input[self.pos..self.pos + len])
            .unwrap()
            .parse()
            .unwrap();
        self.token(len, TokenKind::Num(val))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();
        let tk = match self.peek(0)? {
            b'(' => self.token(1, TokenKind::Lparen),
            b')' => self.token(1, TokenKind::Rparen),
            b'{' => self.token(1, TokenKind::Lbrace),
            b'}' => self.token(1, TokenKind::Rbrace),
            b'<' => match self.peek(1) {
                Some(b'<') => self.token(2, TokenKind::Llangle),
                Some(b'=') => self.token(2, TokenKind::Leq),
                _ => self.token(1, TokenKind::Langle),
            },
            b'>' => match self.peek(1) {
                Some(b'>') => self.token(2, TokenKind::Rrangle),
                Some(b'=') => self.token(2, TokenKind::Geq),
                _ => self.token(1, TokenKind::Rangle),
            },
            b'[' => match self.peek(1) {
                Some(b'[') => self.token(2, TokenKind::Llbracket),
                _ => self.token(1, TokenKind::Lbracket),
            },
            b']' => match self.peek(1) {
                Some(b']') => self.token(2, TokenKind::Rrbracket),
                _ => self.token(1, TokenKind::Rbracket),
            },
            b'+' => self.token(1, TokenKind::Plus),
            b'-' => match self.peek(1) {
                Some(b'>') => self.token(2, TokenKind::Arrow),
                _ => self.token(1, TokenKind::Minus),
            },
            b'*' => self.token(1, TokenKind::Star),
            b'/' => self.token(1, TokenKind::Slash),
            b'&' => match self.peek(1) {
                Some(b'&') => self.token(2, TokenKind::AmpAmp),
                _ => self.token(1, TokenKind::Err),
            },
            b'|' => match self.peek(1) {
                Some(b'|') => self.token(2, TokenKind::PipePipe),
                _ => self.token(1, TokenKind::Err),
            },
            b'^' => self.token(1, TokenKind::Hat),
            b'?' => self.token(1, TokenKind::Question),
            b'!' => match self.peek(1) {
                Some(b'=') => self.token(2, TokenKind::Neq),
                _ => self.token(1, TokenKind::Bang),
            },
            b'=' => match self.peek(1) {
                Some(b'=') => self.token(2, TokenKind::Eq),
                _ => self.token(1, TokenKind::Assign),
            },
            b',' => self.token(1, TokenKind::Comma),
            b'.' => match self.peek(1) {
                Some(b'.') => self.token(2, TokenKind::DotDot),
                _ => self.token(1, TokenKind::Dot),
            },
            b';' => self.token(1, TokenKind::Semi),
            b':' => self.token(1, TokenKind::Colon),
            b'\'' => self.token(1, TokenKind::Prime),
            b'a'..=b'z' | b'A'..=b'Z' => self.lex_alpha(),
            b'0'..=b'9' => self.lex_num(),
            _ => self.token(1, TokenKind::Err),
        };
        Some(tk)
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::lexer::Lexer;
    use std::fmt::Write;

    #[test]
    fn lexing_001() {
        let input = "==4 /* - x (var01 > 0)";
        let lexer = Lexer::new(input.as_bytes());
        let mut res = String::new();
        lexer.for_each(|tk| write!(res, "{tk:?}").unwrap());
        assert_eq!(&res, "'=='(0,2)'4'(2,3)'/'(4,5)'*'(5,6)'-'(7,8)'x'(9,10)'('(11,12)'var01'(12,17)'>'(18,19)'0'(20,21)')'(21,22)")
    }

    #[test]
    fn lexing_002() {
        let input = "  !player ->i [..]<< init>>";
        let lexer = Lexer::new(input.as_bytes());
        let mut res = String::new();
        lexer.for_each(|tk| write!(res, "{tk:?}").unwrap());
        assert_eq!(&res, "'!'(2,3)'player'(3,9)'->'(10,12)'i'(12,13)'['(14,15)'..'(15,17)']'(17,18)'<<'(18,20)'init'(21,25)'>>'(25,27)")
    }
}
