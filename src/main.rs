use crate::common::Edges;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;

mod com;
mod common;
mod edg;

#[derive(Clone)]
struct EmptyGraph {}

impl edg::ExtendedDependencyGraph<i32> for EmptyGraph {
    fn succ(&self, _vert: &i32) -> HashSet<Edges<i32>, RandomState> {
        HashSet::new()
    }
}

fn main() {
    edg::distributed_certain_zero(EmptyGraph {}, 0);
}
