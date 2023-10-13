use super::formula::{Formula, Tree, Zipper};
pub use super::symbol::Atom;
use super::symbol::{Match, Parsable, ParseError, Symbolic};
use pyo3::prelude::*;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
#[pyclass]
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

impl Parsable for PropUnary {}

/// Deriving `PartialOrd` and `Ord` on this enum means that, by ordering the
/// fields in increasing order of precedence, no other work has to be done
/// to make sure the relative precedence of operators is understood.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
#[pyclass]
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

impl Parsable for PropBinary {}

/// Alias for the propositional instantiation of `Formula`.
pub type PropFormula = Formula<PropBinary, PropUnary, Atom>;

/// The Python-bound instance of formula for propositional formulas.
/// This language includes the negation operator and operators for
/// or, and, implication and the biconditional.
#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug)]
#[pyclass]
pub struct Proposition {
    formula: Formula<PropBinary, PropUnary, Atom>,
}

impl Deref for Proposition {
    type Target = PropFormula;

    fn deref(&self) -> &Self::Target {
        &self.formula
    }
}

impl DerefMut for Proposition {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.formula
    }
}

impl From<PropFormula> for Proposition {
    fn from(mut value: PropFormula) -> Self {
        let mut count: usize = 0;
        value.inorder_traverse_mut(&mut |_f| count += 1);
        Proposition { formula: value }
    }
}

impl IntoPy<PyObject> for &mut Proposition {
    fn into_py(self, py: Python<'_>) -> PyObject {
        self.into_py(py)
    }
}

/// These are wrappers around the generic `Formula` methods which allow
/// you to use the builder pattern in Python. Of course you can also use
/// such a pattern with these methods in Rust also!
#[pymethods]
impl Proposition {
    #[new]
    pub fn new(atom: Option<Atom>, s: Option<&str>) -> PyResult<Self> {
        if let Some(val) = s {
            Self::from_str(val)
        } else if let Some(a) = atom {
            Ok(Formula {
                tree: Tree::Atom(a),
                zipper: Zipper::Top,
            }
            .into())
        } else {
            Err(ParseError::EmptyFormula.into())
        }
    }

    pub fn full_merge(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.full_combine(bin, second.formula);
        self
    }

    pub fn full_left_merge(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.full_left_combine(bin, second.formula);
        self
    }

    pub fn place(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.combine(bin, second.formula.tree);
        self
    }

    pub fn left_place(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.left_combine(bin, second.formula.tree);
        self
    }

    pub fn go_up(&mut self) -> &mut Self {
        self.up_zip();
        self
    }
    pub fn go_right(&mut self) -> &mut Self {
        self.right_unzip();
        self
    }

    pub fn go_left(&mut self) -> &mut Self {
        self.left_unzip();
        self
    }

    pub fn go_down(&mut self) -> &mut Self {
        self.down_unzip();
        self
    }

    pub fn turn_right(&mut self) -> &mut Self {
        self.rotate_right();
        self
    }

    pub fn turn_left(&mut self) -> &mut Self {
        self.rotate_left();
        self
    }

    pub fn instance(&mut self, atoms: HashMap<Atom, Self>) -> &mut Self {
        let trees: HashMap<Atom, Tree<PropBinary, PropUnary, Atom>> = atoms
            .into_iter()
            .map(|(k, v)| (k, v.formula.tree))
            .collect();
        self.inorder_traverse_mut(&mut |f: &mut Formula<_, _, _>| f.instantiate(&trees));
        self
    }

    pub fn consume_unary(&mut self, unary: PropUnary) -> &mut Self {
        self.full_unify(unary);
        self
    }

    pub fn append_unary(&mut self, unary: PropUnary) -> &mut Self {
        self.unify(unary);
        self
    }

    pub fn __str__(&self) -> String {
        self.to_string()
    }

    #[staticmethod]
    pub fn from_str(s: &str) -> PyResult<Self> {
        Ok(PropFormula::from_str(s)?.into())
    }
}
