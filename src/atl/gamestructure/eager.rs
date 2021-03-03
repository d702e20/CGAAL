use std::collections::hash_map::RandomState;
use std::collections::HashSet;

use crate::atl::common::{transition_lookup, DynVec, Player, Proposition, State};
use crate::atl::formula::{number, ATLExpressionParser};
use crate::atl::gamestructure::GameStructure;
use pom::parser::Parser;
use std::str::{self};

#[derive(Clone, Debug, Deserialize)]
pub struct EagerGameStructure {
    /// K, number of players
    pub player_count: usize,
    /// Maps states to Vec of atomic proposition, aka the labeling function
    pub labeling: Vec<HashSet<Proposition>>,
    /// Maps states, then players recursively
    pub transitions: Vec<DynVec>,
    /// available moves for a player in a given state
    pub moves: Vec<Vec<usize>>,
}

impl EagerGameStructure {
    /// Returns the number of moves `player` can take when the game is in `state`.
    pub fn available_moves(&self, state: State, player: Player) -> usize {
        *self
            .moves
            .get(state)
            .unwrap_or_else(|| panic!("Requested move for non-existent state {}", state))
            .get(player)
            .unwrap_or_else(|| {
                panic!(
                    "Request move for non-existent player {} from state {}",
                    player, state
                )
            })
    }
}

impl GameStructure for EagerGameStructure {
    fn max_player(&self) -> usize {
        self.player_count
    }

    fn labels(&self, state: State) -> HashSet<Proposition, RandomState> {
        self.labeling
            .get(state)
            .unwrap_or_else(|| panic!("Out of bounds state ({}) given to labeling function", state))
            .clone()
    }

    fn transitions(&self, state: State, choices: Vec<usize>) -> State {
        transition_lookup(
            choices.as_slice(),
            &self
                .transitions
                .get(state)
                .unwrap_or_else(|| panic!("Undefined state {}, no transitions", state)),
        )
    }

    fn move_count(&self, state: State) -> Vec<usize> {
        self.moves
            .get(state)
            .unwrap_or_else(|| panic!("Requested move for non-existent state {}", state))
            .clone()
    }
}

impl ATLExpressionParser for EagerGameStructure {
    fn player_parser(&self) -> Parser<u8, Proposition> {
        // In ATL, players are just their index
        number().convert(move |i| {
            if i <= self.max_player() {
                Ok(i)
            } else {
                Err(format!("Player index '{}' out of bounds.", i))
            }
        })
    }

    fn proposition_parser(&self) -> Parser<u8, Proposition> {
        // In ATL, proposition are just their index. All numbers are valid propositions,
        // but they might not be true anywhere.
        number()
    }
}
