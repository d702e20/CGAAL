use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

pub use eager::EagerGameStructure;

use std::sync::Arc;

mod eager;
pub mod lcgs;

/// Define an index type using the new-type pattern.
/// The type can seamlessly be dereferenced to the underlying type.
macro_rules! index_type {
    ($name:ident, $typ:ty) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
        #[repr(transparent)]
        #[serde(transparent)]
        pub struct $name(pub $typ);

        impl Deref for $name {
            type Target = $typ;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

index_type!(PropIdx, usize);
index_type!(PlayerIdx, usize);
index_type!(StateIdx, usize);
index_type!(ActionIdx, usize);
pub const INVALID_IDX: usize = usize::MAX;

#[allow(clippy::derivable_impls)]
impl Default for StateIdx {
    fn default() -> Self {
        StateIdx(0)
    }
}

pub trait GameStructure {
    fn initial_state_index(&self) -> StateIdx;

    fn player_count(&self) -> usize;

    fn labels(&self, state: StateIdx) -> HashSet<PropIdx>;

    fn get_successor(&self, state: StateIdx, choices: &[ActionIdx]) -> StateIdx;

    /// Returns the number of moves each player can take when the game is in `state`.
    fn move_count(&self, state: StateIdx) -> Vec<usize>;

    /// Returns the human-readable name of the given state
    fn state_name(&self, state: StateIdx) -> String;

    /// Returns the human-readable name of the given proposition
    fn label_name(&self, proposition: PropIdx) -> String;

    /// Returns the human-readable name of the given player
    fn player_name(&self, player: PlayerIdx) -> String;

    /// Returns the human-readable name of the given action by the given player in the given state
    fn action_name(&self, state: StateIdx, player: PlayerIdx, action: ActionIdx) -> String;
}

/// Implements Vec of Vecs in arbitrary runtime determined depth.
#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum DynVec {
    Nest(Vec<Arc<DynVec>>),
    Base(StateIdx),
}

/// Indexes into a DynVec.
/// The length of `choices` must match the depth of `transitions`.
pub(crate) fn transition_lookup(choices: &[ActionIdx], transitions: &DynVec) -> StateIdx {
    match transitions {
        DynVec::Nest(v) => {
            if choices.is_empty() {
                panic!("Fewer choices given than number of players in transitions");
            }

            let choice = choices[0];
            let h: &DynVec = v.get(choice.0).expect("Out of bounds choice");

            transition_lookup(&choices[1..choices.len()], h)
        }
        DynVec::Base(state) => *state,
    }
}
