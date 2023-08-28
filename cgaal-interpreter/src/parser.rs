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
            _ => panic!("Expected a valid command"),
        }
    }
}
