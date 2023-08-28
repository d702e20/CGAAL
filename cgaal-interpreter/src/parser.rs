use crate::lexer::{CGAALLexer, Lexer};
use crate::op::Op;
use crate::token::Token;

pub trait Parser {
    fn parse(input: &str) -> Op;
}

pub struct CGAALParser {}

impl Parser for CGAALParser {
    fn parse(input: &str) -> Op {
        let mut lexer = CGAALLexer::new(input);
        lexer.advance();
        match lexer.curr_token() {
            Token::Start => {
                lexer.advance();
                Op::Skip
            }
            Token::Move => {
                let mut moves = Vec::new();
                lexer.advance();
                while lexer.curr_token() == Token::Number {
                    moves.push(
                        lexer
                            .get_value()
                            .parse::<usize>()
                            .expect("Expected a number"),
                    );
                    lexer.advance();
                }
                Op::Move { moves }
            }
            Token::Players => {
                lexer.advance();
                Op::Players
            }
            _ => panic!(
                "Expected a valid command, the buffer is: {}, and Token is: {}",
                lexer.get_value(),
                lexer.curr_token()
            ),
        }
    }
}
