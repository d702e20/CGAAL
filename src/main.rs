extern crate num_cpus;
#[macro_use]
extern crate serde;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate tracing;

use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::error::Error;
use std::fs::File;
use std::io::{stdout, BufWriter, Read, Write};
use std::process::exit;
use std::sync::Arc;

use clap::{App, Arg, ArgMatches, SubCommand};

use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use crate::atl::formula::Phi;
use crate::atl::gamestructure::EagerGameStructure;
use crate::common::{Edges, VertexAssignment};
use crate::edg::Vertex;
use crate::lcgs::ir::intermediate::IntermediateLCGS;
use crate::lcgs::parse::parse_lcgs;
use crate::printer::print_graph;
use tracing::trace;

mod atl;
mod com;
mod common;
mod distterm;
mod edg;
mod lcgs;
mod printer;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Debug)]
struct EmptyGraph {}

impl Vertex for i32 {}

impl edg::ExtendedDependencyGraph<i32> for EmptyGraph {
    fn succ(&self, _vert: &i32) -> HashSet<Edges<i32>, RandomState> {
        HashSet::new()
    }
}

#[tracing::instrument]
fn main() -> Result<(), Box<dyn Error>> {
    let args = parse_arguments();

    setup_tracing(&args);
    trace!(?args, "commandline arguments");

    match args.subcommand() {
        ("solver", Some(solver_args)) => {
            model_check(solver_args)?;
        }
        ("graph", Some(graph_args)) => {
            let graph = ATLDependencyGraph {
                game_structure: load_json_cgs(graph_args.value_of("model_type").unwrap()),
            };

            let mut file = File::open(graph_args.value_of("formula").unwrap())?;
            let mut formula = String::new();
            file.read_to_string(&mut formula)?;
            let v0 = ATLVertex::FULL {
                state: 0,
                formula: serde_json::from_str(formula.as_str())?,
            };

            let output: Box<dyn Write> = match graph_args.value_of("output") {
                Some(path) => Box::new(File::create(path)?),
                _ => Box::new(stdout()),
            };

            print_graph(graph, v0, output)?;
        }
        _ => (),
    };
    Ok(())
}

/// Tries to perform a model check from the given arguments.
fn model_check(args: &ArgMatches) -> Result<(), Box<dyn Error>> {
    info!(formula = ?args.value_of("formula"), "Model checking on formula");

    // Perform model check using the specified model type
    let result = match args.value_of("model_type") {
        Some("lcgs") => model_check_lazy_cgs(args),
        Some("json") => model_check_json_cgs(args),
        _ => {
            error!(
                "Model type '{:?}' not supported!",
                args.value_of("model_type")
            );
            exit(1)
        }
    }
    .unwrap();

    // Output the result
    match args.value_of("output") {
        Some(path) => {
            let file = File::create(path)?;
            let mut writer = BufWriter::new(&file);
            write!(&mut writer, "Result: {:?}", result)?;
            Ok(())
        }
        _ => {
            println!("Result: {:?}", result);
            Ok(())
        }
    }
}

/// Loads the given "input_model" json file as an EagerGameStructure, then evaluates the given
/// "formula" starting from the state with index 0.
fn model_check_json_cgs(args: &ArgMatches) -> Result<VertexAssignment, Box<dyn Error>> {
    let game_structure = load_json_cgs(args.value_of("input_model").unwrap());

    // Load formula from json file
    let mut file = File::open(args.value_of("formula").unwrap())?;
    let mut formula = String::new();
    file.read_to_string(&mut formula)?;
    let formula: Arc<Phi> = serde_json::from_str(formula.as_str())?;

    Ok(edg::distributed_certain_zero(
        ATLDependencyGraph { game_structure },
        ATLVertex::FULL { state: 0, formula },
        num_cpus::get() as u64,
    ))
}

/// Loads an EagerGameStructure from a json file
fn load_json_cgs(path: &str) -> EagerGameStructure {
    let mut file = File::open(path).unwrap();
    let mut game_structure = String::new();
    file.read_to_string(&mut game_structure).unwrap();
    serde_json::from_str(game_structure.as_str()).unwrap()
}

fn model_check_lazy_cgs_bench(model: &str, formula: &str) {
    let game_structure = load_json_cgs(model);

    // Load formula from json
    let mut file = File::open(formula).unwrap();
    let mut formula = String::new();
    file.read_to_string(&mut formula).unwrap();
    let formula: Arc<Phi> = serde_json::from_str(formula.as_str()).unwrap();

    let v0 = game_structure.initial_state_index();

    // Run algorithm to solve
    Ok(edg::distributed_certain_zero(
        ATLDependencyGraph { game_structure },
        ATLVertex::FULL { state: v0, formula },
        num_cpus::get() as u64,
    ));
}

/// Loads the given "input_model" as an LCGS program, then evaluates the given "formula" starting
/// from the initial state given in the LCGS program.
fn model_check_lazy_cgs(args: &ArgMatches) -> Result<VertexAssignment, Box<dyn Error>> {
    let game_structure = load_lazy_cgs(args.value_of("input_model").unwrap());

    // Load formula from json
    let mut file = File::open(args.value_of("formula").unwrap())?;
    let mut formula = String::new();
    file.read_to_string(&mut formula)?;
    let formula: Arc<Phi> = serde_json::from_str(formula.as_str())?;

    let v0 = game_structure.initial_state_index();

    // Run algorithm to solve
    Ok(edg::distributed_certain_zero(
        ATLDependencyGraph { game_structure },
        ATLVertex::FULL { state: v0, formula },
        num_cpus::get() as u64,
    ))
}

/// Loads an LCGS from a file
fn load_lazy_cgs(path: &str) -> IntermediateLCGS {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    IntermediateLCGS::create(parse_lcgs(&content).unwrap()).unwrap()
}

/// Define and parse command line arguments
fn parse_arguments() -> ArgMatches<'static> {
    fn build_common_arguments<'a>(builder: clap::App<'a, 'a>) -> App<'a, 'a> {
        builder
            .arg(
                Arg::with_name("input_model")
                    .short("m")
                    .long("model")
                    .env("INPUT_MODEL")
                    .required(true)
                    .help("The input file to generate model from"),
            )
            .arg(
                Arg::with_name("model_type")
                    .short("t")
                    .long("model-type")
                    .env("MODEL_TYPE")
                    .required(true)
                    .help("The type of input file given {{lcgs, json}}"),
            )
            .arg(
                Arg::with_name("formula")
                    .short("f")
                    .long("formula")
                    .env("FORMULA")
                    .required(true)
                    .help("The formula to check for"),
            )
            .arg(
                Arg::with_name("output")
                    .short("o")
                    .long("output")
                    .env("OUTPUT")
                    .help("The path to write output to"),
            )
    }

    App::new(PKG_NAME)
        .version(VERSION)
        .author(AUTHORS)
        .arg(
            Arg::with_name("log_filter")
                .short("l")
                .long("log-filter")
                .env("RUST_LOG")
                .default_value("warn")
                .help("Comma separated list of filter directives"),
        )
        .subcommand(build_common_arguments(SubCommand::with_name("solver")))
        .subcommand(build_common_arguments(SubCommand::with_name("graph")))
        .get_matches()
}

fn setup_tracing(args: &ArgMatches) {
    if let Some(filter) = args.value_of("log_filter") {
        let filter = tracing_subscriber::EnvFilter::try_new(filter).unwrap_or_else(|err| {
            eprintln!("Invalid log filter\n{}", err);
            exit(1);
        });
        tracing_subscriber::fmt().with_env_filter(filter).init()
    } else {
        tracing_subscriber::fmt().init()
    }
}
