use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};

#[derive(Clone, Eq, PartialEq)]
struct Lexer<'a> {
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
        self.token(len, TokenKind::Word(word.to_string()))
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
            b'<' => {
                if self.peek(1) == Some(b'=') {
                    self.token(2, TokenKind::Leq)
                } else {
                    self.token(1, TokenKind::Langle)
                }
            }
            b'>' => {
                if self.peek(1) == Some(b'=') {
                    self.token(2, TokenKind::Geq)
                } else {
                    self.token(1, TokenKind::Rangle)
                }
            }
            b'+' => self.token(1, TokenKind::Plus),
            b'-' => self.token(1, TokenKind::Minus),
            b'*' => self.token(1, TokenKind::Star),
            b'/' => self.token(1, TokenKind::Slash),
            b'=' => {
                if self.peek(1) == Some(b'=') {
                    self.token(2, TokenKind::EqEq)
                } else {
                    self.token(1, TokenKind::Eq)
                }
            }
            b'!' => {
                if self.peek(1) == Some(b'=') {
                    self.token(2, TokenKind::Neq)
                } else {
                    self.token(1, TokenKind::Err)
                }
            }
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
    fn lexing001() {
        let input = "==4 /* - x (var01 > 0)";
        let lexer = Lexer::new(input.as_bytes());
        let mut res = String::new();
        lexer.for_each(|tk| write!(res, "{tk:?}").unwrap());
        assert_eq!(
            "'=='(0,2)'4'(2,3)'/'(4,5)'*'(5,6)'-'(7,8)'x'(9,10)'('(11,12)'var01'(12,17)'>'(18,19)'0'(20,21)')'(21,22)",
            &res
        )
    }
}
