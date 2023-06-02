use crate::{ModelAndFormula, SearchStrategyOption};
use atl_checker::algorithms::certain_zero::search_strategy::bfs::BreadthFirstSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::dependency_heuristic::DependencyHeuristicSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::dfs::DepthFirstSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::linear_optimize::LinearOptimizeSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::linear_programming_search::LinearProgrammingSearchBuilder;
use atl_checker::algorithms::certain_zero::search_strategy::{
    SearchStrategy, SearchStrategyBuilder,
};
use atl_checker::algorithms::game_strategy::{model_check, SpecificationProof};
use atl_checker::edg::atledg::vertex::AtlVertex;
use atl_checker::edg::atledg::AtlDependencyGraph;
use atl_checker::game_structure::GameStructure;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::sync::Arc;

/// Solver subcommand
pub fn solver(
    query: ModelAndFormula,
    threads: u64,
    ss: SearchStrategyOption,
    prioritise_back_propagation: bool,
    game_strategy_path: Option<&str>,
    quiet: bool,
) -> Result<(), String> {
    // Dispatching
    match (query, ss) {
        (ModelAndFormula::Json { .. }, SearchStrategyOption::Los) => {
            Err("Linear optimize search is not supported for JSON models".to_string())
        }
        (ModelAndFormula::Json { .. }, SearchStrategyOption::Lps) => {
            Err("Linear programming search is not supported for JSON models".to_string())
        }
        (ModelAndFormula::Json { model, formula }, ss) => {
            let v0 = AtlVertex::Full {
                state: 0,
                formula: Arc::from(formula),
            };
            let graph = AtlDependencyGraph {
                game_structure: model,
            };
            match ss {
                SearchStrategyOption::Bfs => solver_inner(
                    graph,
                    v0,
                    threads,
                    BreadthFirstSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
                SearchStrategyOption::Dfs => solver_inner(
                    graph,
                    v0,
                    threads,
                    DepthFirstSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
                SearchStrategyOption::Dhs => solver_inner(
                    graph,
                    v0,
                    threads,
                    DependencyHeuristicSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
                _ => unreachable!(),
            }
        }
        (ModelAndFormula::Lcgs { model, formula }, ss) => {
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
                        threads,
                        LinearOptimizeSearchBuilder { game: copy },
                        prioritise_back_propagation,
                        game_strategy_path,
                        quiet,
                    )
                }
                SearchStrategyOption::Lps => {
                    let copy = graph.game_structure.clone();
                    solver_inner(
                        graph,
                        v0,
                        threads,
                        LinearProgrammingSearchBuilder { game: copy },
                        prioritise_back_propagation,
                        game_strategy_path,
                        quiet,
                    )
                }
                SearchStrategyOption::Bfs => solver_inner(
                    graph,
                    v0,
                    threads,
                    BreadthFirstSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
                SearchStrategyOption::Dfs => solver_inner(
                    graph,
                    v0,
                    threads,
                    DepthFirstSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
                SearchStrategyOption::Dhs => solver_inner(
                    graph,
                    v0,
                    threads,
                    DependencyHeuristicSearchBuilder,
                    prioritise_back_propagation,
                    game_strategy_path,
                    quiet,
                ),
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
    game_strategy_path: Option<&str>,
    quiet: bool,
) -> Result<(), String> {
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
        game_strategy_path.is_some(),
    );

    if !quiet {
        println!("Model satisfies formula: {}", &result.satisfied);
    }

    // Partial game strategy?
    if let Some(game_strategy_path) = game_strategy_path {
        let proof_res = result.proof.unwrap();
        match proof_res {
            Ok(proof) => match proof {
                SpecificationProof::Strategy(strategy) => {
                    let mut file = File::create(game_strategy_path).map_err(|err| {
                        format!("Failed to create game strategy output file.\n{}", err)
                    })?;
                    write!(file, "{}", strategy.in_context_of(&game_structure))
                        .map_err(|err| format!("Failed to write game strategy file. {}", err))?;
                    if !quiet {
                        println!("Proving game strategy was saved to {}", game_strategy_path);
                    }
                }
                SpecificationProof::NoStrategyNeeded => {
                    if !quiet {
                        println!("No game strategy was computed since a strategy is not needed to prove the given query.");
                    }
                }
            },
            Err(err) => {
                if !quiet {
                    println!("Game strategy was not computed due to error: {}", err);
                }
            }
        }
    }

    if result.satisfied {
        std::process::exit(42);
    } else {
        std::process::exit(43);
    }
}
