
/// The subcommands available
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub enum SubcommandOption {
    #[default]
    Check,
    Index,
    Analyse,
    #[cfg(feature = "graph-printer")]
    Graph
}

/// The formula types that the system supports
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum FormulaFormat {
    Json,
    Atl,
}

/// The model types that the system supports
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ModelFormat {
    Json,
    Lcgs,
}

/// Valid search strategies options
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub enum SearchStrategyOption {
    /// Breadth-first search
    #[default]
    Bfs,
    /// Depth-first search
    Dfs,
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

/// The options that can be passed to the CLI
#[derive(Debug, Default)]
pub struct CliOptions {
    // General
    pub subcommand: SubcommandOption,
    pub quiet: bool,

    // Paths
    pub model_path: String,
    pub model_explicit_format: Option<ModelFormat>,
    pub formula_path: String,
    pub formula_explicit_format: Option<FormulaFormat>,
    pub output_path: Option<String>,
    pub witness_strategy_path: Option<String>,

    // Verification options
    pub threads: u64,
    pub search_strategy: SearchStrategyOption,
    pub prioritise_back_propagation: bool,
    pub use_global: bool,
}