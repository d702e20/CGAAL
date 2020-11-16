#[macro_use]
extern crate log;
extern crate log4rs;
extern crate num_cpus;
#[macro_use]
extern crate serde;

use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use std::sync::Arc;

use clap::{App, Arg, ArgMatches};
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

mod atl;
mod com;
mod common;
mod edg;
mod lcgs;
#[cfg(feature = "graph-printer")]
mod printer;

const PKG_NAME: &'static str = env!("CARGO_PKG_NAME");
const AUTHORS: &'static str = env!("CARGO_PKG_AUTHORS");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Debug)]
struct EmptyGraph {}

impl Vertex for i32 {}

impl edg::ExtendedDependencyGraph<i32> for EmptyGraph {
    fn succ(&self, _vert: &i32) -> HashSet<Edges<i32>, RandomState> {
        HashSet::new()
    }
}

fn main() {
    let args = parse();

    // Load config for logging to stdout and logfile.
    if let Ok(_handle) = log4rs::init_config(get_log4rs_config(
        args.value_of("log_path"),
        match args.value_of("log-level").unwrap().to_lowercase().as_str() {
            "error" => LevelFilter::Error,
            "warn" => LevelFilter::Warn,
            "info" => LevelFilter::Info,
            "debug" => LevelFilter::Debug,
            "trace" => LevelFilter::Trace,
            "off" => LevelFilter::Off,
            level => {
                eprintln!(
                    "Log-level was not in {{error, warn, info, debug, trace, off}}, received: {:?}",
                    level
                );
                exit(1);
            }
        },
    )) {
        model_check(args); // not sure whether this requires match on Result
    }
}

fn model_check(args: ArgMatches) -> Result<(), Box<dyn Error>> {
    info!("Model checking on formula: {:?}", args.value_of("formula"));

    let mut file = File::open(args.value_of("json_model").unwrap())?;
    let mut game_structure = String::new();
    file.read_to_string(&mut game_structure)?;
    let game_structure: EagerGameStructure = serde_json::from_str(game_structure.as_str())?;

    let mut file = File::open(args.value_of("json_formula").unwrap())?;
    let mut formula = String::new();
    file.read_to_string(&mut formula)?;
    let formula: Arc<Phi> = serde_json::from_str(formula.as_str())?;

    let graph = ATLDependencyGraph { game_structure };

    let result = edg::distributed_certain_zero(
        graph,
        ATLVertex::FULL { state: 0, formula },
        num_cpus::get() as u64,
    );
    println!("{:?}", result);

    Ok(())
}

/// Define and parse command line arguments
fn parse() -> ArgMatches<'static> {
    App::new(PKG_NAME)
        .version(VERSION)
        .author(AUTHORS)
        .arg(
            Arg::with_name("formula")
                .short("f")
                .long("formula")
                .env("FORMULA")
                .help("The formula to check for"),
        )
        .arg(
            Arg::with_name("input_file")
                .short("i")
                .long("input")
                .env("INPUT_FILE")
                .help("The input file to generate model from"),
        )
        .arg(
            Arg::with_name("json_model")
                .short("j")
                .long("json-model")
                .env("INPUT_JSON")
                .help("Path to the model in JSON format"),
        )
        .arg(
            Arg::with_name("json_formula")
                .short("r")
                .long("json-formula")
                .env("JSON_FORMULA")
                .help("Path to a JSON formatted formula that will be checked against the model"),
        )
        .arg(
            Arg::with_name("log-level")
                .short("o")
                .long("log-level")
                .env("LOG_LEVEL")
                .default_value("warn")
                .help("{error, warn, info, debug, trace, off}"),
        )
        .arg(
            Arg::with_name("log_path")
                .short("g")
                .long("log-path")
                .env("LOG_PATH")
                .help("Specify the log-file path"),
        )
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
