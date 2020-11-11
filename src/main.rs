extern crate num_cpus;

use crate::common::Edges;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use clap::{App, Arg};

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
    let args = App::new("OnTheFlyATL")
        .version("0.1.0")
        .author("d702e20 <d702e20@cs.aau.dk>")
        .arg(Arg::with_name("prop")
            .short("p")
            .long("prop")
            .env("PROPOSITION")
            .help("The proposition to check for")
        ).arg(Arg::with_name("input_file")
        .short("i")
        .long("input")
        .env("INPUT_FILE")
        .help("The input file to generate model from")).get_matches();

    edg::distributed_certain_zero(EmptyGraph {}, 0, num_cpus::get() as u64);
}
