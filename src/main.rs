#[macro_use]
extern crate serde;
extern crate num_cpus;

use crate::common::Edges;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use crate::atl::gamestructure::EagerGameStructure;
use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use crate::atl::formula::Phi;
use std::sync::Arc;
use std::fs::File;
use std::io::Read;
use std::error::Error;

mod atl;
mod com;
mod common;
mod edg;
mod lcgs;

fn main() -> Result<(), Box<dyn Error>>{
    let mut file = File::open("./test.json")?;
    let mut mexican_standoff = String::new();
    file.read_to_string(&mut mexican_standoff)?;

    let game_structure: EagerGameStructure = serde_json::from_str(mexican_standoff.as_str()).unwrap();
    let graph = ATLDependencyGraph {
        game_structure,
    };

    let result = edg::distributed_certain_zero(graph, ATLVertex::FULL {
        state: 0,
        formula: Arc::new(Phi::PROPOSITION(0)),
    }, num_cpus::get() as u64);
    println!("{:?}", result);

    Ok(())
}
