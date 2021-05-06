use crate::edg::atledg::pmoves::{PartialMove, PartialMoveChoice};
use crate::game_structure::{GameStructure, State};
use joinery::prelude::*;
use std::fmt::{Display, Formatter};

/// A triple of a partial move with its game structure and the relevant state, allowing us to
/// print the move using the names of players and moves as defined by the game structure.
pub struct PartialMoveWithFormatting<'a, G: GameStructure> {
    pub pmove: &'a PartialMove,
    pub game: &'a G,
    pub state: &'a State,
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
                .filter_map(|(player, mov)| {
                    if let PartialMoveChoice::SPECIFIC(mov) = mov {
                        Some(format!(
                            "{}",
                            self.game.action_name(*self.state, player, *mov)
                        ))
                    } else {
                        None
                    }
                })
                .join_with(", ")
                .to_string()
        )
    }
}
