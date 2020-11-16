use crate::atl::common::{Player, Proposition, State};
use std::collections::HashSet;

mod eager;

#[allow(unused_imports)]
pub use eager::EagerGameStructure;

pub trait GameStructure {
    fn max_player(&self) -> u32;

    fn labels(&self, state: State) -> HashSet<Proposition>;

    fn transitions(&self, state: State, choices: Vec<usize>) -> State;

    /// Returns the number of moves `player` can take when the game is in `state`.
    fn available_moves(&self, state: State, player: Player) -> u32;

    /// Returns the number of moves each player can take when the game is in `state`.
    fn move_count(&self, state: State) -> Vec<u32>;
}
