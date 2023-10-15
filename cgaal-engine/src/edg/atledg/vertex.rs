use crate::atl::Phi;
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::Vertex;
use crate::game_structure::StateIdx;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum AtlVertex {
    Full {
        state: StateIdx,
        formula: Arc<Phi>,
    },
    Partial {
        state: StateIdx,
        partial_move: PartialMove,
        formula: Arc<Phi>,
    },
}

impl Display for AtlVertex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AtlVertex::Full { state, formula } => write!(f, "state={} formula={}", state, formula),
            AtlVertex::Partial {
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

impl AtlVertex {
    pub fn state(&self) -> StateIdx {
        match self {
            AtlVertex::Full { state, .. } => *state,
            AtlVertex::Partial { state, .. } => *state,
        }
    }

    pub fn formula(&self) -> Arc<Phi> {
        match self {
            AtlVertex::Full { formula, .. } => formula.clone(),
            AtlVertex::Partial { formula, .. } => formula.clone(),
        }
    }

    pub fn partial_move(&self) -> Option<&PartialMove> {
        match self {
            AtlVertex::Full { .. } => None,
            AtlVertex::Partial { partial_move, .. } => Some(partial_move),
        }
    }

    pub fn is_full(&self) -> bool {
        matches!(self, AtlVertex::Full { .. })
    }
}

impl Vertex for AtlVertex {}
