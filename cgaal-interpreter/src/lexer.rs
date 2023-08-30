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
    /// Returns the current token
    fn curr_token(&self) -> Token {
        self.curr_token.clone()
    }

    /// Advances the lexer to the next recognisable token
    fn advance(&mut self) {
        self.curr_token = self.get_token();
    }

    /// Returns the value of the buffer as a string
    fn get_value(&self) -> String {
        self.buffer.clone().into_iter().collect::<String>()
    }

    /// Initiate the lexer given a string
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
    /// Returns the next recognisable token
    fn get_token(&mut self) -> Token {
        // Clear the buffer and check if the input string is empty
        self.buffer.clear();
        if self.input.is_empty() {
            // If empty return End-Of-File
            return Token::Eof;
        }

        // Try to read the next character
        let check_c = self.get_next_char();
        if check_c.is_none() {
            return Token::Eof;
        }

        // Skip all whitespaces
        let mut c = *check_c.unwrap();
        match self.skip_whitespace(c) {
            None => return Token::Eof,
            Some(ch) => c = ch,
        }

        // Loop through all consecutive alphanumeric characters
        loop {
            if char::is_alphanumeric(c) {
                // Add each alphanumeric character to the buffer
                self.buffer.push(c);
            } else {
                // If a non-alphanumeric character is read, we check on the current buffer
                // if it matches any SYMBOL if it doe so return matching token
                if let Some(token) = Token::get_token_from(self.get_value().as_str()) {
                    return token;
                }
                // If the buffer can be parsed as an usize return a number
                if self.get_value().parse::<usize>().is_ok() {
                    return Token::Number;
                }
                // else it must be a identifier
                return Token::Identifier;
            }

            // Continue the loop
            if let Some(ch) = self.get_next_char() {
                c = *ch;
            } else {
                return Token::Eof;
            }
        }
    }

    /// Updates the current position and returns the next character
    fn get_next_char(&mut self) -> Option<&char> {
        self.curr_position += 1;
        self.input.get(self.curr_position - 1)
    }

    /// Skipping all whitespace char defined in the rust reference, this includes space, newline, tabs etc.
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
