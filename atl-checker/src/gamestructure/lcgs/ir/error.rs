use crate::gamestructure::lcgs::ir::relabeling::RelabelError;
use crate::gamestructure::lcgs::ir::symbol_checker::SymbolError;
use std::fmt::{Display, Formatter};

/// Error produced during the construction of the intermediate representation of LCGS programs.
#[derive(Debug)]
pub enum Error {
    Symbol(SymbolError),
    Relabel(RelabelError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Error::Symbol(SymbolError { msg }) => write!(f, "Error: {}", msg),
            Error::Relabel(RelabelError { msg }) => write!(f, "Error: {}", msg),
        }
    }
}

impl From<SymbolError> for Error {
    fn from(e: SymbolError) -> Self {
        Error::Symbol(e)
    }
}

impl From<RelabelError> for Error {
    fn from(e: RelabelError) -> Self {
        Error::Relabel(e)
    }
}
