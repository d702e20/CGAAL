use serde::__private::fmt::Display;
use std::fmt::Formatter;

/// Error produced when computation of partial game strategies fail
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UnsupportedFormula,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Formulae with nested path qualifiers are not supported.")
    }
}
