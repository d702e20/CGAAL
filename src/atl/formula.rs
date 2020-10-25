use std::sync::Arc;
use crate::atl::common::{Proposition, Player};

#[derive(Hash, Eq, PartialEq, Clone)]
pub(crate) enum Phi {
    TRUE,
    PROPOSITION(Proposition),
    NOT(Arc<Phi>),
    AND(Arc<Phi>, Arc<Phi>),
    NEXT(Player, Arc<Phi>),
    UNTIL {
        player: Player,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
}