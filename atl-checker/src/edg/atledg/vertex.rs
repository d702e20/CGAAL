use crate::atl::Phi;
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::Vertex;
use crate::game_structure::State;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum ATLVertex {
    FULL {
        state: State,
        formula: Arc<Phi>,
    },
    PARTIAL {
        state: State,
        partial_move: PartialMove,
        formula: Arc<Phi>,
    },
}

impl Display for ATLVertex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ATLVertex::FULL { state, formula } => write!(f, "state={} formula={}", state, formula),
            ATLVertex::PARTIAL {
                state,
                partial_move,
                formula,
            } => {
                write!(f, "state={} pmove=[", state)?;
                for (i, choice) in partial_move.0.iter().enumerate() {
                    std::fmt::Display::fmt(&choice, f)?;
                    if i < partial_move.0.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                write!(f, "] formula={}", formula)
            }
        }
    }
}

impl ATLVertex {
    pub fn state(&self) -> State {
        match self {
            ATLVertex::FULL { state, .. } => *state,
            ATLVertex::PARTIAL { state, .. } => *state,
        }
    }

    pub fn formula(&self) -> Arc<Phi> {
        match self {
            ATLVertex::FULL { formula, .. } => formula.clone(),
            ATLVertex::PARTIAL { formula, .. } => formula.clone(),
        }
    }
}

impl Vertex for ATLVertex {}
