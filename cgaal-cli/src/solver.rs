use crate::load::Model;
use crate::options::{CliOptions, SearchStrategyOption};
use cgaal_engine::algorithms::certain_zero::search_strategy::bfs::BreadthFirstSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::dependency_heuristic::DependencyHeuristicSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::dfs::DepthFirstSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::instability_heuristic_search::InstabilityHeuristicSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::linear_optimize::LinearOptimizeSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::linear_programming_search::LinearProgrammingSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::linear_representative_search::LinearRepresentativeSearchBuilder;
use cgaal_engine::algorithms::certain_zero::search_strategy::{
    SearchStrategy, SearchStrategyBuilder,
};
use cgaal_engine::algorithms::game_strategy::{model_check, WitnessStrategy};
use cgaal_engine::atl::Phi;
use cgaal_engine::edg::atledg::vertex::AtlVertex;
use cgaal_engine::edg::atledg::AtlDependencyGraph;
use cgaal_engine::game_structure::GameStructure;
use humantime::format_duration;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::sync::Arc;
use std::time::Instant;

/// Solver subcommand
/// Will exit on success
pub fn solver(model: Model, formula: Phi, options: CliOptions) -> Result<(), String> {
    match (model, options.search_strategy) {
        (Model::Json(_), SearchStrategyOption::Los) => {
            Err("Linear optimize search is not supported for JSON models".to_string())
        }
        (Model::Json(_), SearchStrategyOption::Lps) => {
            Err("Linear programming search is not supported for JSON models".to_string())
        }
        (Model::Json(_), SearchStrategyOption::Ihs) => {
            Err("Instability heuristic search is not supported for JSON models".to_string())
        }
        (Model::Json(_), SearchStrategyOption::Lrs) => {
            Err("Linear representative search is not supported for JSON models".to_string())
        }
        (Model::Json(model), ss) => {
            let v0 = AtlVertex::Full {
                state: model.initial_state_index(),
                formula: Arc::from(formula),
            };
            let graph = AtlDependencyGraph {
                game_structure: model,
            };
            match ss {
                SearchStrategyOption::Bfs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    BreadthFirstSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                SearchStrategyOption::Dfs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    DepthFirstSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                SearchStrategyOption::Dhs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    DependencyHeuristicSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                _ => unreachable!("A search strategy has not been defined for json models"),
            }
        }
        (Model::Lcgs(model), ss) => {
            let v0 = AtlVertex::Full {
                state: model.initial_state_index(),
                formula: Arc::from(formula),
            };
            let graph = AtlDependencyGraph {
                game_structure: model,
            };
            match ss {
                SearchStrategyOption::Los => {
                    let copy = graph.game_structure.clone();
                    solver_inner(
                        graph,
                        v0,
                        options.threads,
                        LinearOptimizeSearchBuilder { game: copy },
                        options.prioritise_back_propagation,
                        options.witness_strategy_path.as_deref(),
                        options.quiet,
                    )
                }
                SearchStrategyOption::Lps => {
                    let copy = graph.game_structure.clone();
                    solver_inner(
                        graph,
                        v0,
                        options.threads,
                        LinearProgrammingSearchBuilder { game: copy },
                        options.prioritise_back_propagation,
                        options.witness_strategy_path.as_deref(),
                        options.quiet,
                    )
                }
                SearchStrategyOption::Bfs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    BreadthFirstSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                SearchStrategyOption::Dfs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    DepthFirstSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                SearchStrategyOption::Dhs => solver_inner(
                    graph,
                    v0,
                    options.threads,
                    DependencyHeuristicSearchBuilder,
                    options.prioritise_back_propagation,
                    options.witness_strategy_path.as_deref(),
                    options.quiet,
                ),
                SearchStrategyOption::Ihs => {
                    let copy = graph.game_structure.clone();
                    solver_inner(
                        graph,
                        v0,
                        options.threads,
                        InstabilityHeuristicSearchBuilder { game: copy },
                        options.prioritise_back_propagation,
                        options.witness_strategy_path.as_deref(),
                        options.quiet,
                    )
                }
                SearchStrategyOption::Lrs => {
                    let copy = graph.game_structure.clone();
                    solver_inner(
                        graph,
                        v0,
                        options.threads,
                        LinearRepresentativeSearchBuilder::new(copy),
                        options.prioritise_back_propagation,
                        options.witness_strategy_path.as_deref(),
                        options.quiet,
                    )
                }
            }
        }
    }
}

fn solver_inner<
    G: GameStructure + Clone + Debug + Send + Sync + 'static,
    S: SearchStrategy<AtlVertex> + Send + 'static,
    SB: SearchStrategyBuilder<AtlVertex, S>,
>(
    edg: AtlDependencyGraph<G>,
    v0: AtlVertex,
    worker_count: u64,
    ss_builder: SB,
    prioritise_back_propagation: bool,
    witness_strategy_path: Option<&str>,
    quiet: bool,
) -> Result<(), String> {
    let now = Instant::now();
    let game_structure = edg.game_structure.clone();
    if !quiet {
        println!(
            "Checking the formula: {}",
            v0.formula().in_context_of(&edg.game_structure)
        );
    }
    let result = model_check(
        edg,
        v0,
        worker_count,
        ss_builder,
        prioritise_back_propagation,
        witness_strategy_path.is_some(),
    );

    if !quiet {
        println!(
            "Time elapsed model checking: {}ms ({})",
            now.elapsed().as_millis(),
            format_duration(now.elapsed())
        );
        println!("Model satisfies formula: {}", &result.satisfied);
    }

    // Partial game strategy?
    if let Some(game_strategy_path) = witness_strategy_path {
        let proof_res = result.proof.unwrap();
        match proof_res {
            Ok(proof) => match proof {
                WitnessStrategy::Strategy(strategy) => {
                    let mut file = File::create(game_strategy_path).map_err(|err| {
                        format!("Failed to create game strategy output file.\n{}", err)
                    })?;
                    write!(file, "{}", strategy.in_context_of(&game_structure))
                        .map_err(|err| format!("Failed to write game strategy file. {}", err))?;
                    if !quiet {
                        println!("Witness strategy was saved to {}", game_strategy_path);
                    }
                }
                WitnessStrategy::NoStrategyExist => {
                    if !quiet {
                        println!("No witness strategy was computed since no strategy exist.");
                    }
                }
                WitnessStrategy::NoStrategyNeeded => {
                    if !quiet {
                        println!("No witness strategy was computed since a strategy is not needed to prove the given query.");
                    }
                }
            },
            Err(err) => {
                if !quiet {
                    println!("Game strategy was not computed due to error: {}", err);
                }
                std::process::exit(1);
            }
        }
    }
    std::process::exit(0);
}
