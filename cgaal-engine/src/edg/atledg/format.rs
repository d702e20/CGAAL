use crate::edg::atledg::pmoves::{PartialMove, PartialMoveChoice};
use crate::game_structure::{GameStructure, PlayerIdx, StateIdx};
use joinery::prelude::*;
use std::fmt::{Display, Formatter};

/// A triple of a partial move with its game structure and the relevant state, allowing us to
/// print the move using the names of players and moves as defined by the game structure.
pub struct PartialMoveWithFormatting<'a, G: GameStructure> {
    pub pmove: &'a PartialMove,
    pub game: &'a G,
    pub state: &'a StateIdx,
}

impl<'a, G: GameStructure> Display for PartialMoveWithFormatting<'a, G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.pmove
                .0
                .iter()
                .enumerate()
                .map(|(player, mov)| {
                    if let PartialMoveChoice::Specific(mov) = mov {
                        self.game.action_name(*self.state, PlayerIdx(player), *mov)
                    } else {
                        "_".to_string()
                    }
                })
                .join_with(", ")
        )
    }
}
