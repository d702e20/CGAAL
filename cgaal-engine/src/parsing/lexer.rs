
#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    // Delimiters
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Langle,
    Rangle,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    EqEq,
    Neq,
    Geq,
    Leq,

    // Other symbols
    Eq,

    // Literals
    Num(i32),
    Word(String),

    // Utility
    Err,
}

struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Lexer {
            input,
            pos: 0,
        }
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
    fn token(&mut self, len: usize, token: Token) -> Token {
        self.pos += len;
        token
    }

    fn lex_alpha(&mut self) -> Token {
        let mut len = 1;
        while self.peek(len).map_or(false, |c| c.is_ascii_alphanumeric() || c == b'_') {
            len += 1;
        }
        let word = std::str::from_utf8(&self.input[self.pos..self.pos + len]).unwrap();
        self.token(len, Token::Word(word.to_string()))
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
        self.token(len, Token::Num(val))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();
        let tk = match self.peek(0)? {
            b'(' => self.token(1, Token::Lparen),
            b')' => self.token(1, Token::Rparen),
            b'{' => self.token(1, Token::Lbrace),
            b'}' => self.token(1, Token::Rbrace),
            b'<' => if self.peek(1) == Some(b'=') {
                self.token(2, Token::Leq)
            } else {
                self.token(1, Token::Langle)
            },
            b'>' => if self.peek(1) == Some(b'=') {
                self.token(2, Token::Geq)
            } else {
                self.token(1, Token::Rangle)
            },
            b'+' => self.token(1, Token::Plus),
            b'-' => self.token(1, Token::Minus),
            b'*' => self.token(1, Token::Star),
            b'/' => self.token(1, Token::Slash),
            b'=' => if self.peek(1) == Some(b'=') {
                self.token(2, Token::EqEq)
            } else {
                self.token(1, Token::Eq)
            },
            b'!' => if self.peek(1) == Some(b'=') {
                self.token(2, Token::Neq)
            } else {
                self.token(1, Token::Err)
            },
            b'a'..=b'z' | b'A'..=b'Z' => self.lex_alpha(),
            b'0'..=b'9' => self.lex_num(),
            _ => self.token(1, Token::Err),
        };
        Some(tk)
    }
}