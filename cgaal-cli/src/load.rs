use cgaal_engine::atl::convert::convert_expr_to_phi;
use cgaal_engine::atl::Phi;
use std::fs::File;
use std::io::Read;

use cgaal_engine::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use cgaal_engine::game_structure::lcgs::parse::parse_lcgs;
use cgaal_engine::game_structure::EagerGameStructure;
use cgaal_engine::parsing::errors::ErrorLog;
use cgaal_engine::parsing::parse_atl;

use crate::options::{FormulaFormat, ModelFormat};

/// An input model in one of the supported formats
#[derive(Debug, Clone)]
pub enum Model {
    Lcgs(IntermediateLcgs),
    Json(EagerGameStructure),
}

impl Model {
    /// Convert to [Option<&IntermediateLcgs>], discarding the value if not LCGS
    pub fn lcgs(&self) -> Option<&IntermediateLcgs> {
        match self {
            Model::Lcgs(lcgs) => Some(lcgs),
            Model::Json(_) => None,
        }
    }

    /// Convert to [Option<&EagerGameStructure>], discarding the value if not JSON
    #[allow(dead_code)]
    pub fn json(&self) -> Option<&EagerGameStructure> {
        match self {
            Model::Lcgs(_) => None,
            Model::Json(json) => Some(json),
        }
    }
}

/// Load a model from a file. The format is inferred if not specified.
pub fn load_model(model_path: &str, model_format: Option<ModelFormat>) -> Result<Model, String> {
    let model_format = model_format.map_or_else(|| infer_model_format(model_path), Ok)?;

    let mut file =
        File::open(model_path).map_err(|err| format!("Failed to open input model. {}", err))?;

    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err(|err| format!("Failed to read input model. {}", err))?;

    match model_format {
        ModelFormat::Json => {
            let game_structure = serde_json::from_str(content.as_str())
                .map_err(|err| format!("Failed to deserialize input model. {}", err))?;

            Ok(Model::Json(game_structure))
        }
        ModelFormat::Lcgs => {
            let lcgs = parse_lcgs(&content)
                .map_err(|err| format!("Failed to parse the LCGS program. {}", err))?;

            let game_structure = IntermediateLcgs::create(lcgs)
                .map_err(|err| format!("Invalid LCGS program. {}", err))?;

            Ok(Model::Lcgs(game_structure))
        }
    }
}

/// Infer the model format from the file extension
fn infer_model_format(model_path: &str) -> Result<ModelFormat, String> {
    if model_path.ends_with(".lcgs") {
        Ok(ModelFormat::Lcgs)
    } else if model_path.ends_with(".json") {
        Ok(ModelFormat::Json)
    } else {
        Err("Cannot infer model format from file the extension. You can specify it with '--model_type=MODEL_TYPE'".to_string())
    }
}

/// Load a formula from a file. The format is inferred if not specified.
/// If the format is ATL, an LCGS game structure must be provided for context.
pub fn load_formula(
    formula_path: &str,
    formula_format: Option<FormulaFormat>,
    game: Option<&IntermediateLcgs>,
) -> Result<Phi, String> {
    let formula_format = formula_format.map_or_else(|| infer_formula_format(formula_path), Ok)?;

    let mut file =
        File::open(formula_path).map_err(|err| format!("Failed to open input formula. {}", err))?;

    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err(|err| format!("Failed to read input formula. {}", err))?;

    match formula_format {
        FormulaFormat::Json => {
            let formula = serde_json::from_str(content.as_str())
                .map_err(|err| format!("Failed to deserialize input formula. {}", err))?;

            Ok(formula)
        }
        FormulaFormat::Atl => {
            let game =
                game.ok_or_else(|| "Cannot load ATL formula without a game structure".to_string())?;

            let mut errors = ErrorLog::new();
            parse_atl(&content, &mut errors)
                .and_then(|expr| convert_expr_to_phi(&expr, game, &mut errors))
                .ok_or_else(|| {
                    format!(
                        "Invalid ATL formula provided:\n{}",
                        errors.to_string(&content)
                    )
                })
        }
    }
}

/// Infer the formula format from the file extension
fn infer_formula_format(formula_path: &str) -> Result<FormulaFormat, String> {
    if formula_path.ends_with(".atl") {
        Ok(FormulaFormat::Atl)
    } else if formula_path.ends_with(".json") {
        Ok(FormulaFormat::Json)
    } else {
        Err("Cannot infer formula format from file the extension. You can specify it with '--formula_type=FORMULA_TYPE'".to_string())
    }
}
