use crate::algorithms::certain_zero::search_strategy::bfs::BreadthFirstSearch;
use crate::algorithms::certain_zero::search_strategy::dependency_heuristic::DependencyHeuristicSearch;
use crate::algorithms::certain_zero::search_strategy::dfs::DepthFirstSearch;
use crate::algorithms::certain_zero::search_strategy::instability_heuristic_search::InstabilityHeuristicSearch;
use crate::algorithms::certain_zero::search_strategy::linear_optimize::LinearOptimizeSearch;
use crate::algorithms::certain_zero::search_strategy::linear_programming_search::LinearProgrammingSearch;
use crate::algorithms::certain_zero::search_strategy::linear_representative_search::{
    LinearRepresentativeSearch, LinearRepresentativeSearchBuilder,
};
use crate::algorithms::certain_zero::search_strategy::rdfs::RandomDepthFirstSearch;
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::intermediate::IntermediateLcgs;
use std::cell::RefCell;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CompositeSearchStrategyOption {
    /// Breadth-first search
    Bfs,
    /// Depth-first search
    Dfs,
    /// Random Depth-first search
    Rdfs,
    /// Linear optimization search
    Los,
    /// Linear programming search
    Lps,
    /// Linear representative search
    Lrs,
    /// Dependency heuristic search
    Dhs,
    /// Instability heuristic search
    Ihs,
}

pub enum CompositeSearchStrategyInstance {
    Bfs(BreadthFirstSearch<AtlVertex>),
    Dfs(DepthFirstSearch<AtlVertex>),
    Rdfs(RandomDepthFirstSearch<AtlVertex>),
    Los(LinearOptimizeSearch),
    Lps(LinearProgrammingSearch),
    Lrs(LinearRepresentativeSearch),
    Dhs(DependencyHeuristicSearch<AtlVertex>),
    Ihs(InstabilityHeuristicSearch),
}

impl SearchStrategy<AtlVertex> for CompositeSearchStrategyInstance {
    fn next(&mut self) -> Option<Edge<AtlVertex>> {
        use CompositeSearchStrategyInstance as CssInst;
        match self {
            CssInst::Bfs(ss) => ss.next(),
            CssInst::Dfs(ss) => ss.next(),
            CssInst::Rdfs(ss) => ss.next(),
            CssInst::Los(ss) => ss.next(),
            CssInst::Lps(ss) => ss.next(),
            CssInst::Lrs(ss) => ss.next(),
            CssInst::Dhs(ss) => ss.next(),
            CssInst::Ihs(ss) => ss.next(),
        }
    }

    fn queue_new_edges(&mut self, edges: Vec<Edge<AtlVertex>>) {
        use CompositeSearchStrategyInstance as CssInst;
        match self {
            CssInst::Bfs(ss) => ss.queue_new_edges(edges),
            CssInst::Dfs(ss) => ss.queue_new_edges(edges),
            CssInst::Rdfs(ss) => ss.queue_new_edges(edges),
            CssInst::Los(ss) => ss.queue_new_edges(edges),
            CssInst::Lps(ss) => ss.queue_new_edges(edges),
            CssInst::Lrs(ss) => ss.queue_new_edges(edges),
            CssInst::Dhs(ss) => ss.queue_new_edges(edges),
            CssInst::Ihs(ss) => ss.queue_new_edges(edges),
        }
    }
}

/// The [CompositeSearchStrategyBuilder] sequentially produces search strategies from a vec of
/// selected search strategies and thus makes it possible to use of different search
/// strategies across different worker. A search strategy can be selected multiple times.
///
/// E.g. if the selected strategies are BFS, BFS, RDFS, then every third worker will use the
/// RDFS strategy, while the remaining workers will use the BFS strategy.
pub struct CompositeSearchStrategyBuilder {
    pub game: IntermediateLcgs,
    pub strategies: Vec<CompositeSearchStrategyOption>,
    pub index: RefCell<usize>,
}

impl CompositeSearchStrategyBuilder {
    pub fn new(
        game: IntermediateLcgs,
        strategies: Vec<CompositeSearchStrategyOption>,
    ) -> CompositeSearchStrategyBuilder {
        assert!(
            strategies.len() > 0,
            "Composite search strategy must have at least one component search strategy"
        );
        CompositeSearchStrategyBuilder {
            game,
            strategies,
            index: RefCell::new(0),
        }
    }

    /// Create a new [CompositeSearchStrategyBuilder] employing each search strategy evenly.
    pub fn one_of_each(game: IntermediateLcgs) -> CompositeSearchStrategyBuilder {
        use CompositeSearchStrategyOption::*;
        // General/best strategies first in case the user is using few workers
        Self::new(game, vec![Dhs, Rdfs, Ihs, Bfs, Lrs, Dfs, Los, Lps])
    }
}

impl SearchStrategyBuilder<AtlVertex, CompositeSearchStrategyInstance>
    for CompositeSearchStrategyBuilder
{
    fn build(&self, root: &AtlVertex) -> CompositeSearchStrategyInstance {
        use CompositeSearchStrategyInstance as CssInst;
        use CompositeSearchStrategyOption as CssOpt;
        let ss = match self.strategies[*self.index.borrow()] {
            CssOpt::Bfs => CssInst::Bfs(BreadthFirstSearch::new()),
            CssOpt::Dfs => CssInst::Dfs(DepthFirstSearch::new()),
            CssOpt::Rdfs => CssInst::Rdfs(RandomDepthFirstSearch::new()),
            CssOpt::Los => CssInst::Los(LinearOptimizeSearch::new(self.game.clone())),
            CssOpt::Lps => CssInst::Lps(LinearProgrammingSearch::new(self.game.clone())),
            CssOpt::Lrs => {
                CssInst::Lrs(LinearRepresentativeSearchBuilder::new(self.game.clone()).build(root))
            }
            CssOpt::Dhs => CssInst::Dhs(DependencyHeuristicSearch::new()),
            CssOpt::Ihs => CssInst::Ihs(InstabilityHeuristicSearch::new(self.game.clone())),
        };
        let mut i = self.index.borrow_mut();
        *i = (*i + 1) % self.strategies.len();
        return ss;
    }
}

#[cfg(test)]
mod test {
    use crate::algorithms::certain_zero::search_strategy::composite::{CompositeSearchStrategyBuilder, CompositeSearchStrategyInstance, CompositeSearchStrategyOption};
    use crate::algorithms::certain_zero::search_strategy::SearchStrategyBuilder;
    use crate::atl::Phi;
    use crate::edg::atledg::vertex::AtlVertex;
    use crate::game_structure::lcgs::intermediate::IntermediateLcgs;
    use crate::game_structure::StateIdx;
    use crate::parsing::ast::LcgsRoot;
    use crate::parsing::errors::ErrorLog;
    use crate::parsing::span::Span;

    #[test]
    fn composite_search_strategy_001() {
        let errors = ErrorLog::new();
        let lcgs = IntermediateLcgs::create(LcgsRoot::new(Span::empty(), vec![]), &errors).unwrap();

        let builder = CompositeSearchStrategyBuilder::new(lcgs, vec![
            CompositeSearchStrategyOption::Bfs,
            CompositeSearchStrategyOption::Dfs,
            CompositeSearchStrategyOption::Dhs,
        ]);

        let root = AtlVertex::Full { state: StateIdx(0), formula: Phi::True.into() };

        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Bfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dhs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Bfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dhs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Bfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dfs(_)));
        assert!(matches!(builder.build(&root), CompositeSearchStrategyInstance::Dhs(_)));
    }
}
