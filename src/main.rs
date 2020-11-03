extern crate num_cpus;
#[macro_use]
extern crate log;
extern crate log4rs;

use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
use crate::atl::formula::Phi;
use crate::atl::gamestructure::EagerGameStructure;
use crate::common::Edges;
use clap::{App, Arg, ArgMatches};
use log::LevelFilter;
use log4rs::append::console::ConsoleAppender;
use log4rs::append::file::FileAppender;
use log4rs::config::{Appender, Config, Root};
use log4rs::encode::pattern::PatternEncoder;
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;

mod atl;
mod com;
mod common;
mod edg;
mod lcgs;
mod printer;

#[derive(Clone, Debug)]
struct EmptyGraph {}

impl edg::ExtendedDependencyGraph<i32> for EmptyGraph {
    fn succ(&self, _vert: &i32) -> HashSet<Edges<i32>, RandomState> {
        HashSet::new()
    }
}

fn main() {
    let args = parse();

    // Load config for logging to stdout and logfile.
    if let Ok(_handle) = log4rs::init_config(get_log4rs_config(
        args.value_of("log-path").unwrap(),
        match args.value_of("log-level").unwrap().to_lowercase().as_str() {
            "error" => LevelFilter::Error,
            "warn" => LevelFilter::Warn,
            "info" => LevelFilter::Info,
            "debug" => LevelFilter::Debug,
            "trace" => LevelFilter::Trace,
            "off" => LevelFilter::Off,
            _ => LevelFilter::Off,
        },
    )) {
        info!("Model checking on formula: {:?}", args.value_of("formula"));

        edg::distributed_certain_zero(EmptyGraph {}, 0, num_cpus::get() as u64);
    }
}

/// Define and parse command line arguments
fn parse() -> ArgMatches<'static> {
    App::new("OnTheFlyATL")
        .version("0.1.0")
        .author("d702e20 <d702e20@cs.aau.dk>")
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
                .long("json")
                .env("INPUT_JSON")
                .help("The json to generate model from"),
        )
        .arg(
            Arg::with_name("json_formula")
                .short("r")
                .long("jsonformula")
                .env("JSON_FORMULA")
                .help("The json to generate formula from"),
        )
        .arg(
            Arg::with_name("log-level")
                .short("o")
                .long("log-level")
                .env("LOG_LEVEL")
                .default_value("info")
                .help("{error, warn, info, debug, trace, off}"),
        )
        .arg(
            Arg::with_name("log-path")
                .short("g")
                .long("log-path")
                .env("LOG_PATH")
                .default_value("model-checker.log")
                .help("Specify the log-file path"),
        )
        .get_matches()
}

/// Create and return log4rs-config with some default values
fn get_log4rs_config(log_path: &str, default_log_level: LevelFilter) -> log4rs::config::Config {
    // Create a stdout-appender for printing to stdout
    let stdout = ConsoleAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{d} [{l}] - {m}{n}")))
        .build();

    // Create a logfile-appender for printing to file
    let logfile = FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{d} [{l}] - {m}{n}")))
        .build(log_path)
        .unwrap();

    // Create and return a config which incorporates the two built appenders
    // and let both appenders be root loggers with 'info' as log-level
    Config::builder()
        .appender(Appender::builder().build("stdout", Box::new(stdout)))
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(
            Root::builder()
                .appender("stdout")
                .appender("logfile")
                .build(default_log_level),
        )
        .unwrap()
}
