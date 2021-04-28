mod args;

#[no_link]
extern crate git_version;
extern crate num_cpus;

use std::fmt::Debug;
use std::fs::File;
use std::io::{Read, Write};
use std::process::exit;
use std::sync::Arc;

use clap::{App, Arg, ArgMatches, SubCommand};
use git_version::git_version;
use tracing::trace;

use crate::args::CommonArgs;
use atl_checker::algorithms::certain_zero::common::VertexAssignment;
use atl_checker::algorithms::certain_zero::distributed_certain_zero;
use atl_checker::algorithms::certain_zero::search_strategy::bfs::BreadthFirstSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::dfs::DepthFirstSearchBuilder;
use atl_checker::analyse::analyse;
use atl_checker::atl::{ATLExpressionParser, Phi};
use atl_checker::edg::atledg::vertex::ATLVertex;
use atl_checker::edg::atledg::ATLDependencyGraph;
use atl_checker::edg::{ExtendedDependencyGraph, Vertex};
use atl_checker::game_structure::lcgs::ast::DeclKind;
use atl_checker::game_structure::lcgs::ir::intermediate::IntermediateLCGS;
use atl_checker::game_structure::lcgs::ir::symbol_table::Owner;
use atl_checker::game_structure::lcgs::parse::parse_lcgs;
use atl_checker::game_structure::{EagerGameStructure, GameStructure};
#[cfg(feature = "graph-printer")]
use atl_checker::printer::print_graph;

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const GIT_VERSION: &str = git_version!(fallback = "unknown");

/// The formula types that the system supports
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum FormulaFormat {
    JSON,
    ATL,
}

/// The model types that the system supports
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ModelType {
    JSON,
    LCGS,
}

/// Valid search strategies options
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum SearchStrategyOption {
    BFS,
    DFS,
}

impl SearchStrategyOption {
    /// Run the distributed certain zero algorithm using the given search strategy
    pub fn distributed_certain_zero<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    >(
        &self,
        edg: G,
        v0: V,
        worker_count: u64,
    ) -> VertexAssignment {
        match self {
            SearchStrategyOption::BFS => {
                distributed_certain_zero(edg, v0, worker_count, BreadthFirstSearchBuilder)
            }
            SearchStrategyOption::DFS => {
                distributed_certain_zero(edg, v0, worker_count, DepthFirstSearchBuilder)
            }
        }
    }
}

#[tracing::instrument]
fn main() {
    if let Err(msg) = main_inner() {
        println!("{}", msg);
        exit(1);
    }
}

fn main_inner() -> Result<(), String> {
    let args = parse_arguments();

    setup_tracing(&args)?;
    trace!(?args, "commandline arguments");

    match args.subcommand() {
        ("index", Some(index_args)) => {
            // Display the indexes for the players and labels

            let input_model_path = index_args.value_of("input_model").unwrap();
            let model_type = get_model_type_from_args(&index_args)?;

            if model_type != ModelType::LCGS {
                return Err("The 'index' command is only valid for LCGS models".to_string());
            }

            // Open the input model file
            let mut file = File::open(input_model_path)
                .map_err(|err| format!("Failed to open input model.\n{}", err))?;

            // Read the input model from the file into memory
            let mut content = String::new();
            file.read_to_string(&mut content)
                .map_err(|err| format!("Failed to read input model.\n{}", err))?;

            let lcgs = parse_lcgs(&content)
                .map_err(|err| format!("Failed to parse the LCGS program.\n{}", err))?;

            let ir = IntermediateLCGS::create(lcgs)
                .map_err(|err| format!("Invalid LCGS program.\n{}", err))?;

            println!("Players:");
            for player in &ir.get_player() {
                println!("{} : {}", player.get_name(), player.index())
            }

            println!("\nLabels:");
            for label_symbol in &ir.get_labels() {
                let label_decl = ir.get_decl(&label_symbol).unwrap();
                if let DeclKind::Label(label) = &label_decl.kind {
                    if Owner::Global == label_symbol.owner {
                        println!("{} : {}", &label_symbol.name, label.index)
                    } else {
                        println!("{} : {}", &label_symbol, label.index)
                    }
                }
            }
        }
        ("solver", Some(solver_args)) => {
            let input_model_path = solver_args.value_of("input_model").unwrap();
            let model_type = get_model_type_from_args(&solver_args)?;
            let formula_path = solver_args.value_of("formula").unwrap();
            let formula_format = get_formula_format_from_args(&solver_args)?;
            let search_strategy = get_search_strategy_from_args(&solver_args)?;

            // Generic start function for use with `load` that start model checking with `distributed_certain_zero`
            fn check_model<G>(
                graph: ATLDependencyGraph<G>,
                v0: ATLVertex,
                threads: u64,
                ss: SearchStrategyOption,
            ) where
                G: GameStructure + Send + Sync + Clone + Debug + 'static,
            {
                let result = ss.distributed_certain_zero(graph, v0, threads);
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
                formula_format,
                |game_structure, formula| {
                    println!(
                        "Checking the formula: {}",
                        formula.in_context_of(&game_structure)
                    );
                    let v0 = ATLVertex::FULL {
                        state: 0,
                        formula: Arc::from(formula),
                    };
                    let graph = ATLDependencyGraph { game_structure };
                    check_model(graph, v0, threads, search_strategy);
                },
                |game_structure, formula| {
                    println!(
                        "Checking the formula: {}",
                        formula.in_context_of(&game_structure)
                    );
                    let arc = Arc::from(formula);
                    let graph = ATLDependencyGraph { game_structure };
                    let v0 = ATLVertex::FULL {
                        state: graph.game_structure.initial_state_index(),
                        formula: arc,
                    };
                    check_model(graph, v0, threads, search_strategy);
                },
            )?
        }
        ("analyse", Some(analyse_args)) => {
            let input_model_path = analyse_args.value_of("input_model").unwrap();
            let model_type = get_model_type_from_args(&analyse_args)?;
            let formula_path = analyse_args.value_of("formula").unwrap();
            let formula_format = get_formula_format_from_args(&analyse_args)?;

            let output_arg = analyse_args.value_of("output").unwrap();

            fn analyse_and_save<G: ExtendedDependencyGraph<ATLVertex>>(
                edg: &G,
                root: ATLVertex,
                output_path: &str,
            ) -> Result<(), String> {
                let data = analyse(edg, root);
                let json = serde_json::to_string_pretty(&data).expect("Failed to serialize data");
                let mut file = File::create(output_path)
                    .map_err(|err| format!("Failed to create output file.\n{}", err))?;
                file.write_all(json.as_bytes())
                    .map_err(|err| format!("Failed to write to output file.\n{}", err))
            }

            load(
                model_type,
                input_model_path,
                formula_path,
                formula_format,
                |game_structure, formula| {
                    let v0 = ATLVertex::FULL {
                        state: 0,
                        formula: Arc::from(formula),
                    };
                    let graph = ATLDependencyGraph { game_structure };
                    analyse_and_save(&graph, v0, output_arg)
                },
                |game_structure, formula| {
                    let v0 = ATLVertex::FULL {
                        state: game_structure.initial_state_index(),
                        formula: Arc::from(formula),
                    };
                    let graph = ATLDependencyGraph { game_structure };
                    analyse_and_save(&graph, v0, output_arg)
                },
            )??
        }
        #[cfg(feature = "graph-printer")]
        ("graph", Some(graph_args)) => {
            {
                let input_model_path = graph_args.value_of("input_model").unwrap();
                let model_type = get_model_type_from_args(&graph_args)?;
                let formula_path = graph_args.value_of("formula").unwrap();
                let formula_format = get_formula_format_from_args(&graph_args)?;

                // Generic start function for use with `load` that starts the graph printer
                fn print_model<G: GameStructure>(
                    graph: ATLDependencyGraph<G>,
                    v0: ATLVertex,
                    output: Option<&str>,
                ) {
                    use std::io::stdout;
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
                    formula_format,
                    |game_structure, formula| {
                        println!(
                            "Printing graph for: {}",
                            formula.in_context_of(&game_structure)
                        );
                        let v0 = ATLVertex::FULL {
                            state: 0,
                            formula: Arc::from(formula),
                        };
                        let graph = ATLDependencyGraph { game_structure };
                        print_model(graph, v0, graph_args.value_of("output"));
                    },
                    |game_structure, formula| {
                        println!(
                            "Printing graph for: {}",
                            formula.in_context_of(&game_structure)
                        );
                        let arc = Arc::from(formula);
                        let graph = ATLDependencyGraph { game_structure };
                        let v0 = ATLVertex::FULL {
                            state: graph.game_structure.initial_state_index(),
                            formula: arc,
                        };
                        print_model(graph, v0, graph_args.value_of("output"));
                    },
                )?
            }
        }
        _ => (),
    };
    Ok(())
}

/// Reads a formula in JSON format from a file and returns the formula as a string
/// and as a parsed Phi struct.
/// This function will exit the program if it encounters an error.
fn load_formula<A: ATLExpressionParser>(path: &str, format: FormulaFormat, expr_parser: &A) -> Phi {
    let mut file = File::open(path).unwrap_or_else(|err| {
        eprintln!("Failed to open formula file\n\nError:\n{}", err);
        exit(1);
    });

    let mut raw_phi = String::new();
    file.read_to_string(&mut raw_phi).unwrap_or_else(|err| {
        eprintln!("Failed to read formula file\n\nError:\n{}", err);
        exit(1);
    });

    match format {
        FormulaFormat::JSON => serde_json::from_str(raw_phi.as_str()).unwrap_or_else(|err| {
            eprintln!("Failed to deserialize formula\n\nError:\n{}", err);
            exit(1);
        }),
        FormulaFormat::ATL => {
            let result = atl_checker::atl::parse_phi(expr_parser, &raw_phi);
            result.unwrap_or_else(|err| {
                eprintln!("Invalid ATL formula provided:\n\n{}", err);
                exit(1)
            })
        }
    }
}

/// Determine the model type (either "json" or "lcgs") by reading the the
/// --model_type argument or inferring it from the model's path extension.  
fn get_model_type_from_args(args: &ArgMatches) -> Result<ModelType, String> {
    match args.value_of("model_type") {
        Some("lcgs") => Ok(ModelType::LCGS),
        Some("json") => Ok(ModelType::JSON),
        None => {
            // Infer model type from file extension
            let model_path = args.value_of("input_model").unwrap();
            if model_path.ends_with(".lcgs") {
                Ok(ModelType::LCGS)
            } else if model_path.ends_with(".json") {
                Ok(ModelType::JSON)
            } else {
                Err("Cannot infer model type from file the extension. You can specify it with '--model_type=MODEL_TYPE'".to_string())
            }
        }
        Some(model_type) => Err(format!("Invalid model type '{}' specified with --model_type. Use either \"lcgs\" or \"json\" [default is inferred from model path].", model_type)),
    }
}

/// Determine the formula format (either "json" or "atl") by reading the
/// --formula_format argument. If none is given, we try to infer it from the file extension
fn get_formula_format_from_args(args: &ArgMatches) -> Result<FormulaFormat, String> {
    match args.value_of("formula_format") {
        Some("json") => Ok(FormulaFormat::JSON),
        Some("atl") => Ok(FormulaFormat::ATL),
        None => {
            // Infer format from file extension
            let formula_path = args.value_of("formula").unwrap();
            if formula_path.ends_with(".atl") {
                Ok(FormulaFormat::ATL)
            } else if formula_path.ends_with(".json") {
                Ok(FormulaFormat::JSON)
            } else {
                Err("Cannot infer formula format from file the extension. You can specify it with '--model_type=MODEL_TYPE'".to_string())
            }
        },
        Some(format) => Err(format!("Invalid formula format '{}' specified with --formula_format. Use either \"atl\" or \"json\" [default is \"atl\"].", format)),
    }
}

/// Determine the search strategy by reading the --search-strategy argument. Default is BFS.
fn get_search_strategy_from_args(args: &ArgMatches) -> Result<SearchStrategyOption, String> {
    match args.value_of("search_strategy") {
        Some("bfs") => Ok(SearchStrategyOption::BFS),
        Some("dfs") => Ok(SearchStrategyOption::DFS),
        Some(other) => Err(format!("Unknown search strategy '{}'. Valid search strategies are \"bfs\" or \"dfs\" [default is \"bfs\"]", other)),
        // Default value
        None => Ok(SearchStrategyOption::BFS)
    }
}

/// Loads a model and a formula from files, and then call the handler function with the loaded model and formula.
fn load<R, J, L>(
    model_type: ModelType,
    game_structure_path: &str,
    formula_path: &str,
    formula_format: FormulaFormat,
    handle_json: J,
    handle_lcgs: L,
) -> Result<R, String>
where
    J: FnOnce(EagerGameStructure, Phi) -> R,
    L: FnOnce(IntermediateLCGS, Phi) -> R,
{
    // Open the input model file
    let mut file = File::open(game_structure_path)
        .map_err(|err| format!("Failed to open input model.\n{}", err))?;
    // Read the input model from the file into memory
    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err(|err| format!("Failed to read input model.\n{}", err))?;

    // Depending on which model_type is specified, use the relevant parsing logic
    match model_type {
        ModelType::JSON => {
            let game_structure = serde_json::from_str(content.as_str())
                .map_err(|err| format!("Failed to deserialize input model.\n{}", err))?;

            let phi = load_formula(formula_path, formula_format, &game_structure);

            Ok(handle_json(game_structure, phi))
        }
        ModelType::LCGS => {
            let lcgs = parse_lcgs(&content)
                .map_err(|err| format!("Failed to parse the LCGS program.\n{}", err))?;

            let game_structure = IntermediateLCGS::create(lcgs)
                .map_err(|err| format!("Invalid LCGS program.\n{}", err))?;

            let phi = load_formula(formula_path, formula_format, &game_structure);

            Ok(handle_lcgs(game_structure, phi))
        }
    }
}

/// Define and parse command line arguments
fn parse_arguments() -> ArgMatches<'static> {
    let version_text = format!("{} ({})", VERSION, GIT_VERSION);
    let string = AUTHORS.replace(":", "\n");
    let mut app = App::new(PKG_NAME)
        .version(version_text.as_str())
        .author(string.as_str())
        .arg(
            Arg::with_name("log_filter")
                .short("l")
                .long("log-filter")
                .env("RUST_LOG")
                .default_value("warn")
                .help("Comma separated list of filter directives"),
        )
        .subcommand(
            SubCommand::with_name("solver")
                .about("Checks satisfiability of an ATL query on a CGS")
                .add_input_model_arg()
                .add_input_model_type_arg()
                .add_formula_arg()
                .add_formula_format_arg()
                .add_search_strategy_arg()
                .arg(
                    Arg::with_name("threads")
                        .short("r")
                        .long("threads")
                        .env("THREADS")
                        .help("Number of threads to run solver on"),
                ),
        )
        .subcommand(
            SubCommand::with_name("index")
                .about("Prints indexes of LCGS declarations")
                .add_input_model_arg(),
        )
        .subcommand(
            SubCommand::with_name("analyse")
                .about("Analyses a EDG generated from an ATL query and a CGS")
                .add_input_model_arg()
                .add_input_model_type_arg()
                .add_formula_arg()
                .add_formula_format_arg()
                .add_output_arg(true),
        );

    if cfg!(feature = "graph-printer") {
        app = app.subcommand(
            SubCommand::with_name("graph")
                .about(
                    "Outputs a Graphviz DOT graph of the EDG generated from an ATL query and a CGS",
                )
                .add_input_model_arg()
                .add_input_model_type_arg()
                .add_formula_arg()
                .add_formula_format_arg()
                .add_output_arg(false),
        );
    }

    app.get_matches()
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
