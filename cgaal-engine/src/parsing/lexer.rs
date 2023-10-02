use crate::parsing::errors::ErrorLog;
use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};

/// A Lexer that converts a byte slice into a stream of tokens.
/// The Lexer is an iterator over tokens.
pub struct Lexer<'a> {
    /// The original input byte slice.
    input: &'a [u8],
    /// The current position in the input, i.e. the number of consumed bytes.
    pos: usize,
    /// Errors that occurred during lexing.
    errors: &'a ErrorLog,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8], errors: &'a ErrorLog) -> Self {
        Lexer {
            input,
            pos: 0,
            errors,
        }
    }

    fn skip_ws(&mut self) {
        // Whitespace
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
        // Comments
        if self.peek(0) == Some(b'/') {
            let start = self.pos;
            if self.peek(1) == Some(b'/') {
                // Single line comment
                while self.peek(0) != Some(b'\n') && self.pos < self.input.len() {
                    self.pos += 1;
                }
                self.skip_ws();
            } else if self.peek(1) == Some(b'*') {
                // Multi line comment
                let mut depth = 1;
                self.pos += 2;
                while depth > 0 && self.pos < self.input.len() {
                    if self.peek(0) == Some(b'/') && self.peek(1) == Some(b'*') {
                        depth += 1;
                        self.pos += 2;
                    } else if self.peek(0) == Some(b'*') && self.peek(1) == Some(b'/') {
                        depth -= 1;
                        self.pos += 2;
                    } else {
                        self.pos += 1;
                    }
                }
                if depth > 0 {
                    let span = Span::new(start, start + 2);
                    self.errors
                        .log(span, "Unclosed multi-line comment".to_string())
                }
                self.skip_ws();
            }
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

    /// Lexes a word, i.e. a sequence of alphanumeric characters and underscores.
    /// Typically identifiers and keywords.
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

    /// Lexes an integer number, i.e. a sequence of digits.
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

    /// Consume bytes until we find a valid utf8 character.
    /// This allows us to handle emojis and other non-ascii characters as well.
    fn lex_unsupported(&mut self) -> Token {
        let mut len = 1;
        while std::str::from_utf8(&self.input[self.pos..self.pos + len]).is_err() {
            len += 1;
        }
        let e = std::str::from_utf8(&self.input[self.pos..self.pos + len]).unwrap();
        self.token(len, TokenKind::Unsupported(e.to_string()))
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
                _ => self.lex_unsupported(),
            },
            b'|' => match self.peek(1) {
                Some(b'|') => self.token(2, TokenKind::PipePipe),
                _ => self.lex_unsupported(),
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
            _ => self.lex_unsupported(),
        };
        Some(tk)
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::errors::ErrorLog;
    use crate::parsing::lexer::Lexer;
    use crate::parsing::token::{Token, TokenKind};

    #[test]
    fn lexing_001() {
        // Check that the lexer produces the correct tokens with correct spans
        let input = "==4 */ - x (var01 > 0)";
        let errors = ErrorLog::new();
        let lexer = Lexer::new(input.as_bytes(), &errors);
        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Eq, (0..2).into()),
                Token::new(TokenKind::Num(4), (2..3).into()),
                Token::new(TokenKind::Star, (4..5).into()),
                Token::new(TokenKind::Slash, (5..6).into()),
                Token::new(TokenKind::Minus, (7..8).into()),
                Token::new(TokenKind::Word("x".to_string()), (9..10).into()),
                Token::new(TokenKind::Lparen, (11..12).into()),
                Token::new(TokenKind::Word("var01".to_string()), (12..17).into()),
                Token::new(TokenKind::Rangle, (18..19).into()),
                Token::new(TokenKind::Num(0), (20..21).into()),
                Token::new(TokenKind::Rparen, (21..22).into()),
            ]
        );
        assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    }

    #[test]
    fn lexing_002() {
        // Check that the lexer produces the correct tokens with correct spans
        let input = "  !player ->i [..]<< init>>";
        let errors = ErrorLog::new();
        let lexer = Lexer::new(input.as_bytes(), &errors);
        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Bang, (2..3).into()),
                Token::new(TokenKind::KwPlayer, (3..9).into()),
                Token::new(TokenKind::Arrow, (10..12).into()),
                Token::new(TokenKind::Word("i".to_string()), (12..13).into()),
                Token::new(TokenKind::Lbracket, (14..15).into()),
                Token::new(TokenKind::DotDot, (15..17).into()),
                Token::new(TokenKind::Rbracket, (17..18).into()),
                Token::new(TokenKind::Llangle, (18..20).into()),
                Token::new(TokenKind::KwInit, (21..25).into()),
                Token::new(TokenKind::Rrangle, (25..27).into()),
            ]
        );
        assert!(errors.is_empty(), "ErrorLog is not empty: {:?}", errors);
    }
}
