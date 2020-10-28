use crate::atl::common::{transition_lookup, DynVec, Proposition, State};
use crate::atl::gamestructure::GameStructure;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;

#[derive(Clone)]
pub(crate) struct EagerGameStructure {
    /// K, number of players
    pub player_count: u32,
    /// Maps states to Vec of atomic proposition, aka the labeling function
    pub labeling: Vec<HashSet<Proposition>>,
    /// Maps states, then players recursively
    pub transitions: Vec<DynVec>,
    /// available moves for a player in a given state
    pub moves: Vec<Vec<u32>>,
}

impl GameStructure for EagerGameStructure {
    fn max_player(&self) -> u32 {
        self.player_count
    }

    fn labels(&self, state: usize) -> HashSet<Proposition, RandomState> {
        self.labeling
            .get(state)
            .expect(format!("Out of bounds state ({}) given to labeling function", state).as_str())
            .clone()
    }

    fn transitions(&self, state: State, choices: Vec<usize>) -> State {
        transition_lookup(
            choices.as_slice(),
            &self
                .transitions
                .get(state)
                .expect(format!("Undefined state {}, no transitions", state).as_str()),
        )
    }

    fn available_moves(&self, state: usize, player: usize) -> u32 {
        *self
            .moves
            .get(state)
            .expect(format!("Requested move for non-existent state {}", state).as_str())
            .get(player)
            .expect(
                format!(
                    "Request move for non-existent player {} from state {}",
                    player, state
                )
                .as_str(),
            )
    }

    fn move_count(&self, state: usize) -> Vec<u32> {
        self.moves
            .get(state)
            .expect(format!("Requested move for non-existent state {}", state).as_str())
            .clone()
    }
}
