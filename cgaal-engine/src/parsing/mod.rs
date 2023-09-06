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

pub fn parse_atl(input: &str, errors: &mut ErrorLog) -> Result<Expr, ()> {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, errors);
    let expr = parser.expr(0).unwrap_or(Expr::new_error());
    parser.expect_end();
    if errors.has_errors() {
        Err(())
    } else {
        Ok(expr)
    }
}
