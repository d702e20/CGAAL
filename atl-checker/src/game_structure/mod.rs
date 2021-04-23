use std::collections::HashSet;

pub use eager::EagerGameStructure;

use std::sync::Arc;

mod eager;
pub mod lcgs;

pub(crate) type Proposition = usize;
pub(crate) type Player = usize;
pub(crate) type State = usize;
pub(crate) type Action = usize;

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

/// Implements Vec of Vecs in arbitrary runtime determined depth.
#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum DynVec {
    NEST(Vec<Arc<DynVec>>),
    BASE(State),
}

/// Indexes into a DynVec.
/// The length of `choices` must match the depth of `transitions`.
pub(crate) fn transition_lookup(choices: &[usize], transitions: &DynVec) -> State {
    match transitions {
        DynVec::NEST(v) => {
            if choices.is_empty() {
                panic!("Fewer choices given than number of players in transitions");
            }

            let choice = choices[0];
            let h: &DynVec = v.get(choice).expect("Out of bounds choice");

            transition_lookup(&choices[1..choices.len()], h)
        }
        DynVec::BASE(state) => *state,
    }
}
