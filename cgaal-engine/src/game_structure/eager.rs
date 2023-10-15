use std::collections::HashSet;

use crate::game_structure::{transition_lookup, DynVec, GameStructure, PlayerIdx, PropIdx, StateIdx, ActionIdx};

#[derive(Clone, Debug, Deserialize)]
pub struct EagerGameStructure {
    /// Initial state
    #[serde(default)]
    pub initial_state: StateIdx,
    /// K, number of players
    pub player_count: usize,
    /// Maps states to Vec of atomic proposition, aka the labeling function
    pub labeling: Vec<HashSet<PropIdx>>,
    /// Maps states, then players recursively
    pub transitions: Vec<DynVec>,
    /// available moves for a player in a given state
    pub moves: Vec<Vec<usize>>,
}

impl EagerGameStructure {
    /// Returns the number of moves `player` can take when the game is in `state`.
    #[allow(dead_code)]
    pub fn available_moves(&self, state: StateIdx, player: PlayerIdx) -> usize {
        *self
            .moves
            .get(state.0)
            .unwrap_or_else(|| panic!("Requested move for non-existent state {}", state))
            .get(player.0)
            .unwrap_or_else(|| {
                panic!(
                    "Request move for non-existent player {} from state {}",
                    player, state
                )
            })
    }
}

impl GameStructure for EagerGameStructure {
    fn initial_state_index(&self) -> StateIdx {
        self.initial_state
    }

    fn player_count(&self) -> usize {
        self.player_count
    }

    fn labels(&self, state: StateIdx) -> HashSet<PropIdx> {
        self.labeling
            .get(state.0)
            .unwrap_or_else(|| panic!("Out of bounds state ({}) given to labeling function", state))
            .clone()
    }

    fn get_successor(&self, state: StateIdx, choices: &[ActionIdx]) -> StateIdx {
        transition_lookup(
            choices,
            self.transitions
                .get(state.0)
                .unwrap_or_else(|| panic!("Undefined state {}, no transitions", state)),
        )
    }

    fn move_count(&self, state: StateIdx) -> Vec<usize> {
        self.moves
            .get(state.0)
            .unwrap_or_else(|| panic!("Requested move for non-existent state {}", state))
            .clone()
    }

    fn state_name(&self, state: StateIdx) -> String {
        state.to_string()
    }

    fn label_name(&self, proposition: PropIdx) -> String {
        proposition.to_string()
    }

    fn player_name(&self, player: PlayerIdx) -> String {
        player.to_string()
    }

    fn action_name(&self, _state: StateIdx, _player: PlayerIdx, action: ActionIdx) -> String {
        action.0.to_string()
    }
}
