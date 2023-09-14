use crate::options::{
    CliOptions, FormulaFormat, ModelFormat, SearchStrategyOption, SubcommandOption,
};
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};
use git_version::git_version;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const GIT_VERSION: &str = git_version!(fallback = "unknown");

/// Define and parse command line arguments
pub fn parse_arguments() -> Result<CliOptions, String> {
    let version_text = format!("{} ({})", VERSION, GIT_VERSION);
    let string = AUTHORS.replace(':', "\n");
    let mut app = App::new(PKG_NAME)
        .version(version_text.as_str())
        .author(string.as_str())
        .arg(
            Arg::with_name("log_filter")
                .short("l")
                .long("log-filter")
                .env("RUST_LOG")
                .default_value("warn")
                .global(true)
                .help("Comma separated list of filter directives"),
        )
        .subcommand(
            SubCommand::with_name("check")
                .about("Checks satisfiability of an ATL property on a CGS")
                .add_positional_model_path_arg()
                .add_model_format_arg()
                .add_positional_formula_arg()
                .add_formula_format_arg()
                .add_search_strategy_arg()
                .add_witness_strategy_arg()
                .add_quiet_arg()
                .add_threads_arg(),
        )
        .subcommand(
            SubCommand::with_name("index")
                .about("Prints indexes of LCGS declarations")
                .add_positional_model_path_arg(),
        )
        .subcommand(
            SubCommand::with_name("analyse")
                .about("Analyses a EDG generated from an ATL query and a CGS")
                .add_positional_model_path_arg()
                .add_model_format_arg()
                .add_positional_formula_arg()
                .add_formula_format_arg()
                .add_positional_output_arg(true),
        )
        .setting(AppSettings::SubcommandRequiredElseHelp);

    if cfg!(feature = "graph-printer") {
        app = app.subcommand(
            SubCommand::with_name("graph")
                .about(
                    "Outputs a Graphviz DOT graph of the EDG generated from an ATL query and a CGS",
                )
                .add_positional_model_path_arg()
                .add_model_format_arg()
                .add_positional_formula_arg()
                .add_formula_format_arg()
                .add_positional_output_arg(false),
        );
    }

    let arg_matches = app.get_matches();

    setup_tracing(&arg_matches)?;

    // FIXME: Merge solver and global to "check"

    let mut options = CliOptions::default();
    match arg_matches.subcommand() {
        ("check", Some(args)) => {
            options.subcommand = SubcommandOption::Check;
            options.use_global = false;
            options.quiet = args.is_present("quiet");
            options.model_path = args.value_of("model_path").unwrap().to_string();
            options.model_explicit_format = parse_model_format_arg(args)?;
            options.formula_path = args.value_of("formula_path").unwrap().to_string();
            options.formula_explicit_format = parse_formula_format_arg(args)?;
            options.witness_strategy_path =
                args.value_of("witness_out_path").map(|s| s.to_string());
            options.threads = parse_threads_arg(args)?;
            options.search_strategy = parse_search_strategy_arg(args)?;
            options.prioritise_back_propagation =
                !args.is_present("no_prioritised_back_propagation");
            options.use_global = args.is_present("global");
        }
        ("index", Some(args)) => {
            options.subcommand = SubcommandOption::Index;
            options.model_path = args.value_of("model_path").unwrap().to_string();
        }
        ("analyse", Some(args)) => {
            options.subcommand = SubcommandOption::Analyse;
            options.model_path = args.value_of("model_path").unwrap().to_string();
            options.model_explicit_format = parse_model_format_arg(args)?;
            options.formula_path = args.value_of("formula_path").unwrap().to_string();
            options.formula_explicit_format = parse_formula_format_arg(args)?;
            options.output_path = Some(args.value_of("output").unwrap().to_string());
        }
        #[cfg(feature = "graph-printer")]
        ("graph", Some(args)) => {
            options.subcommand = SubcommandOption::Graph;
            options.model_path = args.value_of("model_path").unwrap().to_string();
            options.model_explicit_format = parse_model_format_arg(args)?;
            options.formula_path = args.value_of("formula_path").unwrap().to_string();
            options.formula_explicit_format = parse_formula_format_arg(args)?;
            options.output_path = Some(args.value_of("output").unwrap().to_string());
        }
        _ => unreachable!("Unhandled subcommand"),
    }

    Ok(options)
}

/// Parse the model format argument if given (either "json" or "lcgs")
fn parse_model_format_arg(args: &ArgMatches) -> Result<Option<ModelFormat>, String> {
    match args.value_of("model_format") {
        Some("lcgs") => Ok(Some(ModelFormat::Lcgs)),
        Some("json") => Ok(Some(ModelFormat::Json)),
        None => Ok(None),
        Some(other) => Err(format!("Invalid model type '{}' specified with --model_type. Use either \"lcgs\" or \"json\" [default is inferred from model path].", other)),
    }
}

/// Parse the formula format argument if given (either "json" or "atl")
fn parse_formula_format_arg(args: &ArgMatches) -> Result<Option<FormulaFormat>, String> {
    match args.value_of("formula_format") {
        Some("json") => Ok(Some(FormulaFormat::Json)),
        Some("atl") => Ok(Some(FormulaFormat::Atl)),
        Some(other) => Err(format!("Invalid formula type '{}' specified with --formula_type. Use either \"atl\" or \"json\" [inferred if unspecified].", other)),
        None => Ok(None),
    }
}

/// Parse the search strategy argument if given. Default is BFS.
fn parse_search_strategy_arg(args: &ArgMatches) -> Result<SearchStrategyOption, String> {
    match args.value_of("search_strategy") {
        Some("bfs") => Ok(SearchStrategyOption::Bfs),
        Some("dfs") => Ok(SearchStrategyOption::Dfs),
        Some("dhs") => Ok(SearchStrategyOption::Dhs),
        Some("los") => Ok(SearchStrategyOption::Los),
        Some("lps") => Ok(SearchStrategyOption::Lps),
        Some("ihs") => Ok(SearchStrategyOption::Ihs),
        Some("lrs") => Ok(SearchStrategyOption::Lrs),
        Some(other) => Err(format!("Unknown search strategy '{}'. Valid search strategies are bfs, dfs, lps, los, dhs, ihs, lrs  [default is bfs]", other)),
        // Default value
        None => Ok(SearchStrategyOption::Bfs)
    }
}

/// Parse the threads argument if given. Default is the number of CPUs.
fn parse_threads_arg(args: &ArgMatches) -> Result<u64, String> {
    args.value_of("threads")
        .map_or_else(|| Ok(num_cpus::get() as u64), |v| v.parse())
        .map_err(|err| format!("Invalid number of threads. {}", err))
}

fn setup_tracing(args: &ArgMatches) -> Result<(), String> {
    // Configure a filter for tracing data if one have been set
    if let Some(filter) = args.value_of("log_filter") {
        let filter = tracing_subscriber::EnvFilter::try_new(filter)
            .map_err(|err| format!("Invalid log filter.\n{}", err))?;
        tracing_subscriber::fmt().with_env_filter(filter).init()
    } else {
        tracing_subscriber::fmt().init()
    }
    Ok(())
}

/// Trait that allows us to easily add common arguments to the CLI, avoiding duplicate code while
/// remaining flexible in terms of which subcommands have which arguments
pub(crate) trait CommonArgs {
    fn add_positional_model_path_arg(self) -> Self;
    fn add_model_format_arg(self) -> Self;
    fn add_positional_formula_arg(self) -> Self;
    fn add_formula_format_arg(self) -> Self;
    fn add_positional_output_arg(self, required: bool) -> Self;
    fn add_search_strategy_arg(self) -> Self;
    fn add_witness_strategy_arg(self) -> Self;
    fn add_quiet_arg(self) -> Self;
    fn add_threads_arg(self) -> Self;
}

/// Add the common arguments to clap::App
impl CommonArgs for App<'_, '_> {
    /// Adds model path as required positional argument
    fn add_positional_model_path_arg(self) -> Self {
        self.arg(
            Arg::with_name("model_path")
                .help("Path to input model")
                .required(true),
        )
    }

    /// Adds "--model-format" as an optional argument
    fn add_model_format_arg(self) -> Self {
        self.arg(
            Arg::with_name("model_format")
                .long("model-format")
                .help("The format of the input model {{lcgs, json}}"),
        )
    }

    /// Adds formula path as a required positional argument
    fn add_positional_formula_arg(self) -> Self {
        self.arg(
            Arg::with_name("formula_path")
                .help("Path to input formula")
                .required(true),
        )
    }

    /// Adds "--formula-format" as an optional argument
    fn add_formula_format_arg(self) -> Self {
        self.arg(
            Arg::with_name("formula_format")
                .long("formula-format")
                .help("The format of the input formula {{json, atl}}"),
        )
    }

    /// Adds output as a positional argument, optionally required
    fn add_positional_output_arg(self, required: bool) -> Self {
        self.arg(
            Arg::with_name("output")
                .help("The path to write output to")
                .required(required),
        )
    }

    /// Adds "--search-strategy", "--no-prioritised-back-propagation" and "--global" as arguments
    fn add_search_strategy_arg(self) -> Self {
        self.arg(
            Arg::with_name("search_strategy")
                .short("s")
                .long("search-strategy")
                .help("The search strategy used {{bfs, dfs, los, lps, dhs, ihs, lrs}}"),
        )
        .arg(
            Arg::with_name("no_prioritised_back_propagation")
                .long("no-prioritised-back-propagation")
                .takes_value(false)
                .help("Turn off prioritised back-propagation"),
        )
        .arg(
            Arg::with_name("global")
                .long("global")
                .help("Use the global algorithm")
                .conflicts_with("search_strategy")
                .conflicts_with("no_prioritised_back_propagation"),
        )
    }

    /// Adds "--find-strategy" as an argument
    fn add_witness_strategy_arg(self) -> Self {
        self.arg(
            Arg::with_name("witness_out_path")
                .short("g")
                .long("find-strategy")
                .takes_value(true)
                .help("Compute witness strategy along with result and save at given path"),
        )
    }

    /// Adds "-q"/"--quiet" as an argument
    fn add_quiet_arg(self) -> Self {
        self.arg(
            Arg::with_name("quiet")
                .short("q")
                .takes_value(false)
                .long("quiet")
                .help("Suppress stdout"),
        )
    }

    /// Adds "-n"/"--threads" as an argument
    fn add_threads_arg(self) -> Self {
        self.arg(
            Arg::with_name("threads")
                .short("n")
                .long("threads")
                .help("Number of threads to use"),
        )
    }
}
