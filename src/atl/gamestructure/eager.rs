use crate::atl::gamestructure::GameStructure;
use crate::atl::common::{Proposition, State, DynVec, transition_lookup};
use std::collections::HashSet;
use std::collections::hash_map::RandomState;

#[derive(Clone)]
struct EagerGameStructure {
    /// K, number of players
    player_count: u32,
    /// Maps states to Vec of atomic proposition, aka the labeling function
    labeling: Vec<HashSet<Proposition>>,
    /// Maps states, then players recursively
    transitions: Vec<DynVec>,
    /// available moves for a player in a given state
    moves: Vec<Vec<u32>>,
}

impl<'a> GameStructure<'a> for EagerGameStructure {
    fn max_player(&self) -> u32 {
        self.player_count
    }

    fn labels(&self, state: usize) -> &'a HashSet<Proposition, RandomState> {
        todo!()
        //self.labeling.get(state).expect(format!("").as_str())
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
}