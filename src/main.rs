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
use std::fmt::Debug;
use std::fs::File;
use std::io::{stdout, Read, Write};
use std::process::exit;
use std::sync::Arc;

use clap::{App, Arg, ArgMatches, SubCommand};

use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use crate::atl::formula::Phi;
use crate::atl::gamestructure::{EagerGameStructure, GameStructure};
use crate::common::Edges;
use crate::edg::{distributed_certain_zero, Vertex};
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

    let subargs = args.subcommand().1.unwrap();

    let formula_path = subargs.value_of("formula").unwrap();
    let input_model_path = subargs.value_of("input_model").unwrap();
    let model_type = match subargs.value_of("model_type") {
        Some("lcgs") => "lcgs",
        Some("json") => "json",
        None => {
            // Infer model type from file extension
            let model_path = subargs.value_of("input_model").unwrap();
            if model_path.ends_with(".lcgs") {
                "lcgs"
            } else if model_path.ends_with(".json") {
                "json"
            } else {
                eprintln!(
                    "Cannot infer model type from file the extension. You can specify it with '--model_type=MODEL_TYPE'"
                );
                exit(1)
            }
        }
        Some(model_type) => {
            eprintln!("Model type '{:?}' is not supported", model_type);
            exit(1);
        }
    };

    match args.subcommand() {
        ("solver", Some(solver_args)) => {
            // Generic start function for use with `load` that start model checking with `distributed_certain_zero`
            fn check_model<G>(graph: ATLDependencyGraph<G>, v0: ATLVertex, threads: u64)
            where
                G: GameStructure + Send + Sync + Clone + Debug + 'static,
            {
                let result = distributed_certain_zero(graph, v0, threads);
                println!("Result: {}", result);
            }

            let threads = match solver_args.value_of("threads") {
                None => num_cpus::get() as u64,
                Some(t_arg) => t_arg.parse().unwrap(),
            };

            load(
                model_type,
                input_model_path,
                formula_path,
                |graph, formula| {
                    let v0 = ATLVertex::FULL { state: 0, formula };
                    check_model(graph, v0, threads);
                },
                |graph, formula| {
                    let v0 = ATLVertex::FULL {
                        state: graph.game_structure.initial_state_index(),
                        formula,
                    };
                    check_model(graph, v0, threads);
                },
            )
        }
        ("graph", Some(_args)) => {
            // Generic start function for use with `load` that starts the graph printer
            fn print_model<G: GameStructure>(
                graph: ATLDependencyGraph<G>,
                v0: ATLVertex,
                output: Option<&str>,
            ) {
                let output: Box<dyn Write> = match output {
                    Some(path) => {
                        let file = File::create(path).unwrap_or_else(|err| {
                            eprintln!("Failed to create output file\n\nError:\n{}", err);
                            exit(1);
                        });
                        Box::new(file)
                    }
                    _ => Box::new(stdout()),
                };

                print_graph(graph, v0, output).unwrap();
            }

            load(
                model_type,
                input_model_path,
                formula_path,
                |graph, formula| {
                    let v0 = ATLVertex::FULL { state: 0, formula };
                    print_model(graph, v0, subargs.value_of("output"));
                },
                |graph, formula| {
                    let v0 = ATLVertex::FULL {
                        state: graph.game_structure.initial_state_index(),
                        formula,
                    };
                    print_model(graph, v0, subargs.value_of("output"));
                },
            )
        }
        _ => (),
    };
    Ok(())
}

/// Reads a formula in JSON format from a file.
/// This function will exit the program if it encounters an error.
fn load_formula(path: &str) -> Arc<Phi> {
    let mut file = File::open(path).unwrap_or_else(|err| {
        eprintln!("Failed to open formula file\n\nError:\n{}", err);
        exit(1);
    });
    let mut formula = String::new();
    file.read_to_string(&mut formula).unwrap_or_else(|err| {
        eprintln!("Failed to read formula file\n\nError:\n{}", err);
        exit(1);
    });
    serde_json::from_str(formula.as_str()).unwrap_or_else(|err| {
        eprintln!("Failed to deserialize formula\n\nError:\n{}", err);
        exit(1);
    })
}

/// Loads a model and a formula from files, and then call the handler function with the loaded model and formula.
fn load<R, J, L>(
    model_type: &str,
    game_structure_path: &str,
    formula_path: &str,
    handle_json: J,
    handle_lcgs: L,
) -> R
where
    J: FnOnce(ATLDependencyGraph<EagerGameStructure>, Arc<Phi>) -> R,
    L: FnOnce(ATLDependencyGraph<IntermediateLCGS>, Arc<Phi>) -> R,
{
    // Open the input model file
    let mut file = File::open(game_structure_path).unwrap_or_else(|err| {
        eprintln!("Failed to open input model\n\nError:\n{}", err);
        exit(1);
    });
    // Read the input model from the file into memory
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap_or_else(|err| {
        eprintln!("Failed to read input model\n\nError:\n{}", err);
        exit(1);
    });

    // Depending on which model_type is specified, use the relevant parsing logic
    match model_type {
        "json" => {
            let game_structure = serde_json::from_str(content.as_str()).unwrap_or_else(|err| {
                eprintln!("Failed to deserialize input model\n\nError:\n{}", err);
                exit(1);
            });
            let graph = ATLDependencyGraph { game_structure };

            let formula = load_formula(formula_path);

            handle_json(graph, formula)
        }
        "lcgs" => {
            let lcgs = parse_lcgs(&content).unwrap_or_else(|err| {
                eprintln!("Failed to parse the LCGS program\n\nError:\n{}", err);
                exit(1);
            });
            let game_structure = IntermediateLCGS::create(lcgs).unwrap_or_else(|_err| {
                eprintln!("Invalid LCGS program");
                exit(1);
            });
            let graph = ATLDependencyGraph { game_structure };

            let formula = load_formula(formula_path);

            handle_lcgs(graph, formula)
        }
        &_ => {
            eprintln!("Model type '{:?}' not supported", model_type);
            exit(1)
        }
    }
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
        .subcommand(build_common_arguments(
            SubCommand::with_name("solver").arg(
                Arg::with_name("threads")
                    .short("r")
                    .long("threads")
                    .env("THREADS")
                    .help("Number of threads to run solver on"),
            ),
        ))
        .subcommand(build_common_arguments(SubCommand::with_name("graph")))
        .get_matches()
}

fn setup_tracing(args: &ArgMatches) {
    // Configure a filter for tracing data if one have been set
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
