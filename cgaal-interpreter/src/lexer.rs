use crate::token::Token;
use crate::token::SYMBOLS;

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
}

impl Lexer for CGAALLexer {
    fn curr_token(&self) -> Token {
        self.curr_token.clone()
    }
    fn advance(&mut self) {
        self.get_token();
    }
    fn get_value(&self) -> String {
        self.buffer.clone().into_iter().collect::<String>()
    }

    fn new(input: &str) -> Self {
        Self {
            input: Vec::from(input.chars().collect::<Vec<char>>()),
            buffer: Vec::new(),
            curr_token: Token::Start,
        }
    }
}

impl CGAALLexer {
    fn get_token(&mut self) -> Token {
        self.buffer.clear();
        if self.input.is_empty() {
            return Token::EOF;
        }
        let mut c = self.get_next_char();
        c = self.skip_whitespace(c);
        loop {
            if char::is_alphanumeric(c) {
                self.buffer.push(c);
            } else if char::is_whitespace(c) || self.input.is_empty() {
                if SYMBOLS.contains_key(self.get_value().as_str()) {
                    return SYMBOLS[self.get_value().as_str()].clone();
                } else if self.get_value().parse::<usize>().is_ok() {
                    return Token::Number;
                }
                return Token::Identifier;
            }
            c = self.get_next_char()
        }
    }

    fn get_next_char(&mut self) -> char {
        self.input.pop().unwrap()
    }

    // Skipping all whitespace char from the rust reference, this includes space, newline, tabs etc.
    fn skip_whitespace(&mut self, mut curr_char: char) -> char {
        while char::is_whitespace(curr_char) {
            curr_char = self.get_next_char();
        }
        curr_char
    }
}
