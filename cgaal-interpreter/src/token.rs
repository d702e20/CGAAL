use phf::phf_map;

#[derive(Clone, Eq, PartialEq)]
pub enum Token {
    Start,
    EOF,
    EOL,
    Identifier,
    Move,
    Number,
}

pub static SYMBOLS: phf::Map<&'static str, Token> = phf_map! {
    "move" => Token::Move,
    "number" => Token::Number,
};

pub fn parse_symbol(symbol: &str) -> Option<Token> {
    SYMBOLS.get(symbol).cloned()
}
