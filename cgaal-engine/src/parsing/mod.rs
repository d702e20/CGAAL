use crate::parsing::ast::Expr;
use crate::parsing::errors::ErrorLog;
use crate::parsing::lexer::Lexer;
use crate::parsing::parser::Parser;

pub mod ast;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;
mod token;

pub fn parse_atl(input: &str, errors: &mut ErrorLog) -> Option<Expr> {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, errors);
    let expr = parser
        .expr(0)
        .and_then(|expr| {
            parser.expect_end();
            Ok(expr)
        })
        .unwrap_or(Expr::new_error());
    if errors.has_errors() {
        None
    } else {
        Some(expr)
    }
}
