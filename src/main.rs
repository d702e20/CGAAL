extern crate num_cpus;

use crate::common::Edges;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;

mod atl;
mod com;
mod common;
mod edg;
mod lcgs;

#[derive(Clone, Debug)]
struct EmptyGraph {}

impl edg::ExtendedDependencyGraph<i32> for EmptyGraph {
    fn succ(&self, _vert: &i32) -> HashSet<Edges<i32>, RandomState> {
        HashSet::new()
    }
}

fn main() {
    edg::distributed_certain_zero(EmptyGraph {}, 0, num_cpus::get() as u64);
}
