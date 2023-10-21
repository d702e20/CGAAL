use crate::parsing::ast::{Expr, LcgsRoot};
use crate::parsing::errors::{ErrorLog, SeeErrorLog};
use crate::parsing::lexer::Lexer;
use crate::parsing::parser::Parser;

pub mod ast;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;
mod token;

/// Parse an ATL expression.
/// Returns Err if there were errors. See the [ErrorLog] for details.
pub fn parse_atl(input: &str, errors: &ErrorLog) -> Result<Expr, SeeErrorLog> {
    let lexer = Lexer::new(input.as_bytes(), errors);
    let mut parser = Parser::new(lexer, errors);
    let expr = parser
        .expr()
        .map(|expr| {
            parser.expect_end();
            expr
        })
        .unwrap_or(Expr::new_error());
    if errors.has_errors() {
        Err(SeeErrorLog)
    } else {
        Ok(expr)
    }
}

/// Parse an LCGS program.
/// Returns None if there were errors. See the [ErrorLog] for details.
pub fn parse_lcgs(input: &str, errors: &ErrorLog) -> Option<LcgsRoot> {
    let lexer = Lexer::new(input.as_bytes(), errors);
    let mut parser = Parser::new(lexer, errors);
    let lcgs = parser.lcgs_root().map(|lcgs| {
        parser.expect_end();
        lcgs
    });
    if errors.has_errors() {
        None
    } else {
        lcgs.ok()
    }
}
