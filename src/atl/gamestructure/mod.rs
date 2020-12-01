use std::collections::HashSet;

#[allow(unused_imports)]
pub use eager::EagerGameStructure;

use crate::atl::common::{Proposition, State};

mod eager;

pub trait GameStructure {
    fn max_player(&self) -> u32;

    fn labels(&self, state: State) -> HashSet<Proposition>;

    fn transitions(&self, state: State, choices: Vec<usize>) -> State;

    /// Returns the number of moves each player can take when the game is in `state`.
    fn move_count(&self, state: State) -> Vec<u32>;
}
