use crate::algorithms::game_strategy::partial::PartialStrategy;
use crate::game_structure::GameStructure;
use joinery::prelude::*;
use std::fmt::{Display, Formatter};

/// A pairing of a partial strategy with its game structure, allowing us to print
/// the strategy using the names of players and moves as defined by the game structure.
pub struct PartialStrategyWithFormatting<'a, G: GameStructure> {
    pub strategy: &'a PartialStrategy,
    pub game: &'a G,
}

impl<'a, G: GameStructure> Display for PartialStrategyWithFormatting<'a, G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Partial strategy for the player(s): {}",
            self.strategy
                .players
                .iter()
                .map(|p| self.game.player_name(*p))
                .join_with(", ")
        )?;
        for (state, pmove) in &self.strategy.move_to_pick {
            writeln!(
                f,
                "{}: {}",
                self.game.state_name(*state),
                pmove.in_context_of(self.game, state)
            )?;
        }
        Ok(())
    }
}
