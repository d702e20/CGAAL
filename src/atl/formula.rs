use crate::atl::common::{Player, Proposition};
use std::sync::Arc;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Deserialize)]
pub(crate) enum Phi {
    #[serde(rename = "proposition")]
    PROPOSITION(Proposition),
    #[serde(rename = "not")]
    NOT(Arc<Phi>),
    #[serde(rename = "or")]
    OR(Arc<Phi>, Arc<Phi>),
    #[serde(rename = "next")]
    NEXT {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    #[serde(rename = "despite until")]
    DESPITE_UNTIL {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
    #[serde(rename = "enforce until")]
    ENFORCE_UNTIL {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    }
}
