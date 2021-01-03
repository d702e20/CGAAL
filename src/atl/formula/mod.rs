mod parser;

use crate::atl::common::{Player, Proposition};
use joinery::prelude::*;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// Alternating-time Temporal Logic formula
#[derive(Hash, Eq, PartialEq, Clone, Debug, Deserialize, Serialize)]
pub(crate) enum Phi {
    /// Trivially satisfied
    #[serde(rename = "true")]
    True,
    /// Trivially not satisfied
    #[serde(rename = "false")]
    False,
    /// The current state must have the label/proposition
    #[serde(rename = "proposition")]
    Proposition(Proposition),
    /// It must not be the case that subformula is satisfied
    #[serde(rename = "not")]
    Not(Arc<Phi>),
    /// It must be the case that either formula is satisfied
    #[serde(rename = "or")]
    Or(Arc<Phi>, Arc<Phi>),
    /// It must be the case that either formula is satisfied
    #[serde(rename = "and")]
    And(Arc<Phi>, Arc<Phi>),
    /// It must be the case that `formula` is satisfied in the next step despite what actions `players` choose.
    #[serde(rename = "despite next")]
    DespiteNext {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that players can enforce that `formula` is satisfied in the next step
    #[serde(rename = "enforce next")]
    EnforceNext {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that `pre` is satisfied until `until` is satisfied despite what actions `players` choose.
    #[serde(rename = "despite until")]
    DespiteUntil {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
    /// It must be the case that `players` can enforce that `pre` is satisfied until `until` is satisfied
    #[serde(rename = "enforce until")]
    EnforceUntil {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
    /// It must be the case that `formula` is satisfied in some coming step despite what actions `players` choose.
    #[serde(rename = "despite eventually")]
    DespiteEventually {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that `players` can enforce that `formula` is satisfied in some coming step.
    #[serde(rename = "enforce eventually")]
    EnforceEventually {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that `formula` is continually satisfied despite what actions `players` choose.
    #[serde(rename = "despite invariant")]
    DespiteInvariant {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that `players` can enforce that `formula` is continually satisfied.
    #[serde(rename = "enforce invariant")]
    EnforceInvariant {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
}

impl Display for Phi {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Phi::True => write!(f, "true"),
            Phi::False => write!(f, "false"),
            Phi::Proposition(id) => write!(f, "'{}'", id),
            Phi::Not(formula) => write!(f, "¬({})", formula),
            Phi::Or(left, right) => write!(f, "({} ∨ {})", left, right),
            Phi::And(left, right) => write!(f, "({} ∧ {})", left, right),
            Phi::DespiteNext { players, formula } => write!(
                f,
                "⟦{}⟧◯{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
            Phi::EnforceNext { players, formula } => write!(
                f,
                "⟪{}⟫◯{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
            Phi::DespiteUntil {
                players,
                pre,
                until,
            } => write!(
                f,
                "⟦{}⟧({} 𝑼 {})",
                players.iter().join_with(",").to_string(),
                pre,
                until
            ),
            Phi::EnforceUntil {
                players,
                pre,
                until,
            } => write!(
                f,
                "⟪{}⟫({} 𝑼 {})",
                players.iter().join_with(",").to_string(),
                pre,
                until
            ),
            Phi::DespiteEventually { players, formula } => write!(
                f,
                "⟦{}⟧◇{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
            Phi::EnforceEventually { players, formula } => write!(
                f,
                "⟪{}⟫◇{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
            Phi::DespiteInvariant { players, formula } => write!(
                f,
                "⟦{}⟧□{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
            Phi::EnforceInvariant { players, formula } => write!(
                f,
                "⟪{}⟫□{}",
                players.iter().join_with(",").to_string(),
                formula
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::atl::formula::Phi::*;
    use std::sync::Arc;

    #[test]
    fn test_display_01() {
        let formula = EnforceUntil {
            players: vec![0, 1],
            pre: Arc::new(Or {
                0: Arc::new(Proposition(1)),
                1: Arc::new(Not(Arc::new(Proposition(2)))),
            }),
            until: Arc::new(False),
        };
        assert_eq!("⟪0,1⟫(('1' ∨ ¬('2')) 𝑼 false)", format!("{}", formula));
    }
}
