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

/// Parse an ATL expression.
/// Returns None if there were errors. See the error log for details.
pub fn parse_atl(input: &str, errors: &mut ErrorLog) -> Option<Expr> {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer, errors);
    let expr = parser
        .expr()
        .map(|expr| {
            parser.expect_end();
            expr
        })
        .unwrap_or(Expr::new_error());
    if errors.has_errors() {
        None
    } else {
        Some(expr)
    }
}
