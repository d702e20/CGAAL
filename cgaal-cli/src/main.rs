#[no_link]
extern crate git_version;
extern crate num_cpus;

use std::cmp::Ordering;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::process::exit;
use std::sync::Arc;
use std::time::Instant;

use humantime::format_duration;

use cgaal_engine::algorithms::global::multithread::MultithreadedGlobalAlgorithm;
use cgaal_engine::algorithms::global::singlethread::SinglethreadedGlobalAlgorithm;
use cgaal_engine::analyse::analyse;
use cgaal_engine::atl::Phi;
use cgaal_engine::edg::atledg::vertex::AtlVertex;
use cgaal_engine::edg::atledg::AtlDependencyGraph;
use cgaal_engine::game_structure::GameStructure;
use cgaal_engine::parsing::ast::DeclKind;
#[cfg(feature = "graph-printer")]
use cgaal_engine::printer::print_graph;

use crate::args::parse_arguments;
use crate::load::{load_formula, load_model, Model};
use crate::options::SubcommandOption;
use crate::solver::solver;

mod args;
mod load;
mod options;
mod solver;

#[tracing::instrument]
fn main() {
    if let Err(msg) = main_inner() {
        println!("{}", msg);
        exit(1);
    }
}

fn main_inner() -> Result<(), String> {
    let options = parse_arguments()?;

    match options.subcommand {
        SubcommandOption::Index => {
            // Display the indexes for the players and labels

            let model = load_model(options.model_path.as_str(), options.model_explicit_format)?;
            let ir = model
                .lcgs()
                .ok_or("The 'index' command is only valid for LCGS models")?;

            println!("Players:");
            for player in ir.get_players() {
                let decl = ir.get_decl(&player.symbol_index).unwrap();
                println!("{} : {}", decl.ident, player.index);
            }

            println!("\nLabels:");
            for label_symbol in ir.get_labels() {
                let label_decl = ir.get_decl(label_symbol).unwrap();
                if let DeclKind::StateLabel(idx, _) = &label_decl.kind {
                    println!("{} : {}", &label_decl.ident, idx);
                }
            }
        }
        SubcommandOption::Check if !options.use_global => {
            let model = load_model(options.model_path.as_str(), options.model_explicit_format)?;
            let formula = load_formula(
                options.formula_path.as_str(),
                options.formula_explicit_format,
                model.lcgs(),
            )?;
            solver(model, formula, options)?;
        }
        SubcommandOption::Check if !options.use_global => {
            fn run_global<G: GameStructure + Send + Sync + Clone + Debug + 'static>(
                game: G,
                formula: Phi,
                threads: u64,
                quiet: bool,
            ) -> Result<(), String> {
                let v0 = AtlVertex::Full {
                    state: game.initial_state_index(),
                    formula: Arc::from(formula),
                };
                let graph = AtlDependencyGraph {
                    game_structure: game,
                };

                let now = Instant::now();
                let result = match threads.cmp(&1) {
                    Ordering::Less => Err("The number must be a positive integer")?,
                    Ordering::Equal => SinglethreadedGlobalAlgorithm::new(graph, v0).run(),
                    Ordering::Greater => {
                        // The numbers of worker is one less than threads
                        // since the master is running in its own thread.
                        let worker_count = threads - 1;
                        MultithreadedGlobalAlgorithm::new(graph, worker_count, v0).run()
                    }
                };

                if !quiet {
                    println!(
                        "Time elapsed model checking: {}ms ({})",
                        now.elapsed().as_millis(),
                        format_duration(now.elapsed())
                    );
                    println!("Model satisfies formula: {}", result);
                }

                Ok(())
            }

            let model = load_model(options.model_path.as_str(), options.model_explicit_format)?;
            let formula = load_formula(
                options.formula_path.as_str(),
                options.formula_explicit_format,
                model.lcgs(),
            )?;

            match model {
                Model::Lcgs(model) => run_global(model, formula, options.threads, options.quiet)?,
                Model::Json(model) => run_global(model, formula, options.threads, options.quiet)?,
            }
        }
        SubcommandOption::Analyse => {
            fn analyse_and_save<G: GameStructure>(
                game: G,
                formula: Phi,
                output_path: &str,
            ) -> Result<(), String> {
                let root = AtlVertex::Full {
                    state: game.initial_state_index(),
                    formula: Arc::from(formula),
                };
                let edg = AtlDependencyGraph {
                    game_structure: game,
                };
                let data = analyse(&edg, root);
                let json = serde_json::to_string_pretty(&data).expect("Failed to serialize data");
                let mut file = File::create(output_path)
                    .map_err(|err| format!("Failed to create output file. {}", err))?;
                file.write_all(json.as_bytes())
                    .map_err(|err| format!("Failed to write to output file. {}", err))
            }

            let model = load_model(&options.model_path, options.model_explicit_format)?;
            let formula = load_formula(
                &options.formula_path,
                options.formula_explicit_format,
                model.lcgs(),
            )?;

            match model {
                Model::Lcgs(model) => {
                    analyse_and_save(model, formula, options.output_path.as_deref().unwrap())?
                }
                Model::Json(model) => {
                    analyse_and_save(model, formula, options.output_path.as_deref().unwrap())?
                }
            }
        }
        #[cfg(feature = "graph-printer")]
        SubcommandOption::Graph => {
            fn print_model<G: GameStructure>(game: G, formula: Phi, output: Option<&str>) {
                println!("Printing graph for: {}", formula.in_context_of(&game));
                let arc = Arc::from(formula);
                let graph = AtlDependencyGraph {
                    game_structure: game,
                };
                let v0 = AtlVertex::Full {
                    state: graph.game_structure.initial_state_index(),
                    formula: arc,
                };
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

            let model = load_model(options.model_path.as_str(), options.model_explicit_format)?;
            let formula = load_formula(
                options.formula_path.as_str(),
                options.formula_explicit_format,
                model.lcgs(),
            )?;

            match model {
                Model::Lcgs(model) => print_model(model, formula, options.output_path.as_deref()),
                Model::Json(model) => print_model(model, formula, options.output_path.as_deref()),
            }
        }
        _ => (),
    };
    Ok(())
}
