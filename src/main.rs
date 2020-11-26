#![feature(test)]
extern crate test;
#[macro_use]
extern crate log;
extern crate log4rs;
extern crate num_cpus;
#[macro_use]
extern crate serde;
#[macro_use]
extern crate lazy_static;

use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::error::Error;
use std::fs::File;
use std::io::{stdout, BufWriter, Read, Write};
use std::process::exit;
use std::sync::Arc;

use clap::{App, Arg, ArgMatches, SubCommand};
use log::LevelFilter;
use log4rs::append::console::ConsoleAppender;
use log4rs::append::file::FileAppender;
use log4rs::config::{Appender, Config, Root};
use log4rs::encode::pattern::PatternEncoder;

use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use crate::atl::formula::Phi;
use crate::atl::gamestructure::EagerGameStructure;
use crate::common::Edges;
use crate::edg::Vertex;
use crate::printer::print_graph;

mod atl;
mod com;
mod common;
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

fn main() -> Result<(), Box<dyn Error>> {
    let args = parse();

    // Load config for logging to stdout and logfile.
    let _handle = log4rs::init_config(get_log4rs_config(
        args.value_of("log_path"),
        match args.value_of("log-level").unwrap().to_lowercase().as_str() {
            "error" => LevelFilter::Error,
            "warn" => LevelFilter::Warn,
            "info" => LevelFilter::Info,
            "debug" => LevelFilter::Debug,
            "trace" => LevelFilter::Trace,
            "off" => LevelFilter::Off,
            level => {
                eprintln!( // Cannot use logging error-macro before logging is initialised
                           "Log-level was not in {{error, warn, info, debug, trace, off}}, received: {:?}",
                           level
                );
                exit(1);
            }
        },
    ))?;

    match args.subcommand() {
        ("solver", Some(solver_args)) => {
            model_check(solver_args)?;
        }
        ("graph", Some(graph_args)) => {
            let graph = ATLDependencyGraph {
                game_structure: decode_game_structure(graph_args),
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

fn decode_game_structure(args: &ArgMatches) -> EagerGameStructure {
    match args.value_of("model_type") {
        Some("lcgs") => {
            // TODO: implement lgcs parser
            EagerGameStructure {
                player_count: 0,
                labeling: vec![],
                transitions: vec![],
                moves: vec![],
            }
        }
        Some("json") => {
            let mut file = File::open(args.value_of("input_model").unwrap()).unwrap();
            let mut game_structure = String::new();
            file.read_to_string(&mut game_structure).unwrap();
            serde_json::from_str(game_structure.as_str()).unwrap()
        }
        _ => {
            error!(
                "Model type {:?} not supported!",
                args.value_of("model_type")
            );
            exit(1)
        }
    }
}

fn model_check(args: &ArgMatches) -> Result<(), Box<dyn Error>> {
    info!("Model checking on formula: {:?}", args.value_of("formula"));

    let game_structure = decode_game_structure(args);

    let mut file = File::open(args.value_of("formula").unwrap())?;
    let mut formula = String::new();
    file.read_to_string(&mut formula)?;
    let formula: Arc<Phi> = serde_json::from_str(formula.as_str())?;

    let graph = ATLDependencyGraph { game_structure };

    let result = edg::distributed_certain_zero(
        graph,
        ATLVertex::FULL { state: 0, formula },
        num_cpus::get() as u64,
    );

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

/// Define and parse command line arguments
fn parse() -> ArgMatches<'static> {
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
            Arg::with_name("log-level")
                .short("l")
                .long("log-level")
                .env("LOG_LEVEL")
                .default_value("warn")
                .help("{error, warn, info, debug, trace, off}"),
        )
        .arg(
            Arg::with_name("log_path")
                .short("p")
                .long("log-path")
                .env("LOG_PATH")
                .help("Write log to file if log-file path is specified"),
        )
        .subcommand(build_common_arguments(SubCommand::with_name("solver")))
        .subcommand(build_common_arguments(SubCommand::with_name("graph")))
        .get_matches()
}

/// Create and return log4rs-config with some default values
fn get_log4rs_config(
    log_path: Option<&str>,
    default_log_level: LevelFilter,
) -> log4rs::config::Config {
    // Create a stdout-appender for printing to stdout
    let stdout = ConsoleAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{d} [{l}] - {m}{n}")))
        .build();

    // Create and return a config which incorporates the two built appenders
    // and let both appenders be root loggers with 'info' as log-level
    let builder = Config::builder().appender(Appender::builder().build("stdout", Box::new(stdout)));

    // build with or without logfile appender depending on log_path arg
    if let Some(log_path) = log_path {
        // Create a logfile-appender for printing to file
        let logfile = FileAppender::builder()
            .encoder(Box::new(PatternEncoder::new("{d} [{l}] - {m}{n}")))
            .build(log_path)
            .unwrap();
        builder
            .appender(Appender::builder().build("logfile", Box::new(logfile)))
            .build(
                Root::builder()
                    .appender("stdout")
                    .appender("logfile")
                    .build(default_log_level),
            )
            .unwrap()
    } else {
        builder
            .build(Root::builder().appender("stdout").build(default_log_level))
            .unwrap()
    }
}
