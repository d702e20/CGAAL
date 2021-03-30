use std::collections::HashSet;

#[allow(unused_imports)]
pub use eager::EagerGameStructure;

use crate::atl::common::{Action, Player, Proposition, State};

mod eager;

pub trait GameStructure {
    fn max_player(&self) -> usize;

    fn labels(&self, state: State) -> HashSet<Proposition>;

    fn transitions(&self, state: State, choices: Vec<usize>) -> State;

    /// Returns the number of moves each player can take when the game is in `state`.
    fn move_count(&self, state: State) -> Vec<usize>;

    /// Returns the human-readable name of the given state
    fn state_name(&self, state: State) -> String;

    /// Returns the human-readable name of the given proposition
    fn label_name(&self, proposition: Proposition) -> String;

    /// Returns the human-readable name of the given player
    fn player_name(&self, player: Player) -> String;

    /// Returns the human-readable name of the given action by the given player in the given state
    fn action_name(&self, state: State, player: Player, action: Action) -> String;
}
