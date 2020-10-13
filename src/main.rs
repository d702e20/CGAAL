use std::collections::hash_map::RandomState;
use crate::common::Edges;
use std::collections::HashSet;

mod edg;
mod com;
mod common;

struct EmptyGraph {}

impl edg::ExtendedDependencyGraph for EmptyGraph {
    fn succ(&self, _vert: i32) -> HashSet<Edges, RandomState> {
        HashSet::new()
    }
}

fn main() {
    edg::distributed_certain_zero(EmptyGraph{}, 0);
}
