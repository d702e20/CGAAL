use crate::atl::common::{Player, Proposition};
use std::sync::Arc;

#[derive(Hash, Eq, PartialEq, Clone)]
pub(crate) enum Phi {
    PROPOSITION(Proposition),
    NOT(Arc<Phi>),
    OR(Arc<Phi>, Arc<Phi>),
    NEXT {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    DESPITE_UNTIL {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
}
