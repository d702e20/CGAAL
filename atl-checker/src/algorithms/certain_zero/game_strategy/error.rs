use serde::__private::fmt::Display;
use std::fmt::Formatter;

/// Error produced when computation of partial game strategies fail
#[derive(Debug)]
pub enum Error {
    UnsupportedFormula,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cannot compute partial strategy for a formula with nested path qualifiers."
        )
    }
}
