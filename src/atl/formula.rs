use crate::atl::common::{Player, Proposition};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// Alternating-time Temporal Logic formula
#[derive(Hash, Eq, PartialEq, Clone, Debug, Deserialize)]
pub(crate) enum Phi {
    /// The current state must have the label/proposition
    #[serde(rename = "proposition")]
    PROPOSITION(Proposition),
    /// It must not be the case that subformula is satisfied
    #[serde(rename = "not")]
    NOT(Arc<Phi>),
    /// It must be the case that either formula is satisfied
    #[serde(rename = "or")]
    OR(Arc<Phi>, Arc<Phi>),
    /// It must be the case that players can enforce that `formula` is satisfied in the next step
    #[serde(rename = "next")]
    NEXT {
        players: Vec<Player>,
        formula: Arc<Phi>,
    },
    /// It must be the case that `pre` is satisfied until `until` is satisfied despite what actions `players` choose.
    #[serde(rename = "despite until")]
    DESPITE_UNTIL {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
    /// It must be that `players` can enforce that `pre` is satisfied until `until` is satisfied
    #[serde(rename = "enforce until")]
    ENFORCE_UNTIL {
        players: Vec<Player>,
        pre: Arc<Phi>,
        until: Arc<Phi>,
    },
}

impl Display for Phi {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Phi::PROPOSITION(id) => f.write_fmt(format_args!("'{}'", id)),
            Phi::NOT(formula) => {
                f.write_str("¬(")?;
                formula.fmt(f)?;
                f.write_str(")")
            }
            Phi::OR(left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
                f.write_str(") ∨ (")?;
                right.fmt(f)?;
                f.write_str(")")
            }
            Phi::NEXT { players, formula } => {
                f.write_str("⟪")?;
                f.write_str(
                    players
                        .iter()
                        .map(|id| id.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                        .as_str(),
                )?;
                f.write_str("⟫◯[")?;
                formula.fmt(f)?;
                f.write_str("]")
            }
            Phi::DESPITE_UNTIL {
                players,
                pre,
                until,
            } => {
                f.write_str("⟦")?;
                f.write_str(
                    players
                        .iter()
                        .map(|id| id.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                        .as_str(),
                )?;
                f.write_str("⟧((")?;
                pre.fmt(f)?;
                f.write_str(") 𝑼 (")?;
                until.fmt(f)?;
                f.write_str("))")
            }
            Phi::ENFORCE_UNTIL {
                players,
                pre,
                until,
            } => {
                f.write_str("⟪")?;
                f.write_str(
                    players
                        .iter()
                        .map(|id| id.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                        .as_str(),
                )?;
                f.write_str("⟫((")?;
                pre.fmt(f)?;
                f.write_str(") 𝑼 (")?;
                until.fmt(f)?;
                f.write_str("))")
            }
        }
    }
}
