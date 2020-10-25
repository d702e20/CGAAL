use std::collections::HashSet;
use crate::atl::common::{Proposition, State, Player};

mod eager;

pub(crate) trait GameStructure<'a> {
    fn max_player(&self) -> u32;

    fn labels(&self, state: usize) -> &'a HashSet<Proposition>;

    fn transitions(&self, state: State, choices: Vec<usize>) -> State;

    fn available_moves(&self, state: State, player: Player) -> u32;
}