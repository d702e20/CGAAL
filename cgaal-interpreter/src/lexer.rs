use crate::token::Token;

pub trait Lexer {
    fn curr_token(&self) -> Token;
    fn advance(&mut self);
    fn get_value(&self) -> String;
    fn new(input: &str) -> Self;
}

pub struct CGAALLexer {
    input: Vec<char>,
    buffer: Vec<char>,
    curr_token: Token,
    curr_position: usize,
}

impl Lexer for CGAALLexer {
    fn curr_token(&self) -> Token {
        self.curr_token.clone()
    }
    fn advance(&mut self) {
        self.curr_token = self.get_token();
    }
    fn get_value(&self) -> String {
        self.buffer.clone().into_iter().collect::<String>()
    }

    fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect::<Vec<char>>(),
            buffer: Vec::new(),
            curr_token: Token::Start,
            curr_position: 0,
        }
    }
}

impl CGAALLexer {
    fn get_token(&mut self) -> Token {
        self.buffer.clear();
        if self.input.is_empty() {
            return Token::EOF;
        }
        let check_c = self.get_next_char();
        if check_c.is_none() {
            return Token::EOF;
        }
        let mut c = *check_c.unwrap();
        match self.skip_whitespace(c) {
            None => return Token::EOF,
            Some(ch) => c = ch,
        }
        loop {
            if char::is_alphanumeric(c) {
                self.buffer.push(c);
                if let Some(token) = Token::get_token_from(self.get_value().as_str()) {
                    return token;
                }
            } else {
                if self.get_value().parse::<usize>().is_ok() {
                    return Token::Number;
                }
                return Token::Identifier;
            }
            if let Some(ch) = self.get_next_char() {
                c = *ch;
            } else {
                return Token::EOF;
            }
        }
    }

    fn get_next_char(&mut self) -> Option<&char> {
        self.curr_position += 1;
        self.input.get(self.curr_position - 1)
    }

    // Skipping all whitespace char defined in the rust reference, this includes space, newline, tabs etc.
    fn skip_whitespace(&mut self, curr_char: char) -> Option<char> {
        if !char::is_whitespace(curr_char) {
            return Some(curr_char);
        }
        while let Some(c) = self.get_next_char() {
            if !char::is_whitespace(*c) {
                return Some(*c);
            }
        }
        None
    }
}
