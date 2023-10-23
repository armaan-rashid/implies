use super::formula::Formula;
pub use super::symbol::Atom;
use super::symbol::{Match, Symbol, Symbolic};
#[cfg(feature = "python")]
use pyo3::pyclass;
use std::fmt::Display;

/// The negation operator, nothing super remarkable here.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
#[cfg_attr(feature = "python", pyclass)]
pub enum PropUnary {
    #[default]
    Not,
}

impl Display for PropUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropUnary::Not => write!(f, "¬"),
        }
    }
}

impl Symbolic for PropUnary {}

impl Match for PropUnary {
    fn get_match(s: &str) -> Option<Self> {
        match s {
            "¬" | "!" | "~" | "not" => Some(Self::Not),
            _ => None,
        }
    }
}

/// Deriving `PartialOrd` and `Ord` on this enum means that, by ordering the
/// fields in increasing order of precedence, no other work has to be done
/// to make sure the relative precedence of operators is understood.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
#[cfg_attr(feature = "python", pyclass)]
pub enum PropBinary {
    Iff,
    #[default]
    Implies,
    Or,
    And,
}

impl Display for PropBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropBinary::Iff => write!(f, " ↔ "),
            PropBinary::Implies => write!(f, " → "),
            PropBinary::And => write!(f, " ∧ "),
            PropBinary::Or => write!(f, " ∨ "),
        }
    }
}

impl Symbolic for PropBinary {}

impl Match for PropBinary {
    fn get_match(s: &str) -> Option<Self> {
        match s {
            "<->" | "↔" | "iff" => Some(Self::Iff),
            "->" | "→" | "implies" => Some(Self::Implies),
            "\\/" | "∨" | "or" => Some(Self::Or),
            "/\\" | "∧" | "and" => Some(Self::And),
            _ => None,
        }
    }
}

/// Alias for the propositional instantiation of `Formula`.
pub type PropFormula = Formula<PropBinary, PropUnary, Atom>;
pub type PropSymbol = Symbol<PropBinary, PropUnary, Atom>;
