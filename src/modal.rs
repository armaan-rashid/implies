//! A preimplemented specification of modal logic. Hopefully
//! the brevity of this page shows just how easy it is to use
//! `Formula` for your own logic.

use crate::formula::*;
use crate::parser::Match;
use crate::prop::*;
use crate::symbol::{Atom, Symbolic};
use enum_iterator::Sequence;

/// The usual unary operators for modal logic: negation,
/// box and diamond. Most of the traits you need to get your
/// symbol types to work with implies are derivable.
///
/// Pro tip:
/// Write operator enums like this in the ascending precedence order
/// want for your operators, so that deriving Ord freely gives you the
/// precedence you expect. In the case of unary operators like these,
/// it doesn't matter, but it's useful for binary operators.
#[derive(Sequence, PartialEq, Eq, Ord, PartialOrd, Copy, Clone, Default, Hash)]
enum ModalUnary {
    Box,
    Diamond,
    // Give any value as the default, it just allows
    // fast swap operations under the hood without
    // unsafe Rust.
    #[default]
    Not,
}

/// Specify how your type should be pretty-printed.
impl std::fmt::Display for ModalUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModalUnary::Box => write!(f, "◻"),
            ModalUnary::Diamond => write!(f, "◊"),
            ModalUnary::Not => write!(f, "¬"),
        }
    }
}

/// This marker trait shows you've covered all the bases.
impl Symbolic for ModalUnary {}

/// Implement this simple trait (whose methods are partial inverses)
/// for immediate access to parsing formulae from strings.
impl Match for ModalUnary {
    fn match_str(s: &str) -> Option<Self> {
        match s {
            "◻" => Some(ModalUnary::Box),
            "◊" => Some(ModalUnary::Diamond),
            "¬" | "not" | "~" => Some(ModalUnary::Not),
            _ => None,
        }
    }

    fn get_matches(&self) -> Vec<String> {
        match self {
            ModalUnary::Box => vec!["◻".to_owned()],
            ModalUnary::Diamond => vec!["◊".to_owned()],
            ModalUnary::Not => vec!["¬".to_owned(), "not".to_owned(), "~".to_owned()],
        }
    }
}

/// The binary operators for modal logic are the same as those for propositional.
type ModalBinary = PropBinary;

/// Just write a type alias and that's it, all of implies' functionality for free.
type ModalFormula = Formula<ModalBinary, ModalUnary, Atom>;
