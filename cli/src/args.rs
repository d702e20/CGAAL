use clap::{App, Arg};

/// Trait that allows us to easily add common arguments to the CLI, avoiding duplicate code while
/// remaining flexible in terms of which subcommands have which arguments
pub(crate) trait CommonArgs {
    fn add_input_model_arg(self) -> Self;
    fn add_input_model_type_arg(self) -> Self;
    fn add_formula_arg(self) -> Self;
    fn add_formula_type_arg(self) -> Self;
    fn add_output_arg(self, required: bool) -> Self;
    fn add_search_strategy_arg(self) -> Self;
}

/// Add the common arguments to clap::App
impl CommonArgs for App<'_, '_> {
    /// Adds "--input-model" as a required argument
    fn add_input_model_arg(self) -> Self {
        self.arg(
            Arg::with_name("input_model")
                .short("m")
                .long("model")
                .env("INPUT_MODEL")
                .required(true)
                .help("The input file to generate model from"),
        )
    }

    /// Adds "--model-type" as an optional argument
    fn add_input_model_type_arg(self) -> Self {
        self.arg(
            Arg::with_name("model_type")
                .short("t")
                .long("model-type")
                .env("MODEL_TYPE")
                .help("The type of input file given {{lcgs, json}}"),
        )
    }

    /// Adds "--formula" as a required argument
    fn add_formula_arg(self) -> Self {
        self.arg(
            Arg::with_name("formula")
                .short("f")
                .long("formula")
                .env("FORMULA")
                .required(true)
                .help("The formula to check for"),
        )
    }

    /// Adds "--formula-format" as an optional argument
    fn add_formula_type_arg(self) -> Self {
        self.arg(
            Arg::with_name("formula_type")
                .short("y")
                .long("formula-type")
                .env("FORMULA_TYPE")
                .help("The type of ATL formula file given {{json, atl}}"),
        )
    }

    /// Adds "--output" as an argument
    fn add_output_arg(self, required: bool) -> Self {
        self.arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .env("OUTPUT")
                .required(required)
                .help("The path to write output to"),
        )
    }

    /// Adds "--search-strategy" as an argument
    fn add_search_strategy_arg(self) -> Self {
        self.arg(
            Arg::with_name("search_strategy")
                .short("s")
                .long("search-strategy")
                .env("SEARCH_STRATEGY")
                .help("The search strategy used {{bfs, dfs, los, dhs}}"),
        )
        .arg(
            Arg::with_name("no_prioritised_back_propagation")
                .long("no-prioritised-back-propagation")
                .takes_value(false)
                .help("Turn off prioritised back-propagation"),
        )
    }
}
