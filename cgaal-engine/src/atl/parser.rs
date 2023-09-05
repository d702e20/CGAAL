use std::str::{self};

use crate::atl::Phi;

/// Parse an ATL formula
pub fn parse_phi(_input: &str) -> Result<Phi, String> {
    Ok(Phi::True)
}
