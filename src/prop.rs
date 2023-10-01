use super::formula::{Formula, Tree, Zipper};
use super::symbol::{Atom, Match, Parsable, ParseError, Symbolic};
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
    fn str_matches(&self) -> &[&str] {
        match self {
            PropUnary::Not => &["¬", "!", "~", "not"],
        }
    }

    fn get_match(s: &str) -> Option<Self> {
        match s {
            "¬" | "!" | "~" | "not" => Some(Self::Not),
            _ => None,
        }
    }
}

impl Parsable for PropUnary {}

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
    fn str_matches(&self) -> &[&str] {
        match self {
            PropBinary::Iff => &["<->", "↔", "iff"],
            PropBinary::Implies => &["->", "→", "implies"],
            PropBinary::Or => &["\\/", "∨", "or"],
            PropBinary::And => &["/\\", "∧", "and"],
        }
    }

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

pub type PropFormula = Formula<PropBinary, PropUnary, Atom>;

#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug)]
#[pyclass]
pub struct Proposition(Formula<PropBinary, PropUnary, Atom>);

impl Deref for Proposition {
    type Target = PropFormula;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Proposition {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<PropFormula> for Proposition {
    fn from(value: PropFormula) -> Self {
        Proposition(value)
    }
}

impl IntoPy<PyObject> for &mut Proposition {
    fn into_py(self, py: Python<'_>) -> PyObject {
        self.into_py(py)
    }
}

#[pymethods]
impl Proposition {
    #[new]
    fn new(atom: Option<Atom>, s: Option<&str>) -> PyResult<Self> {
        if let Some(val) = s {
            Self::read(val)
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

    fn merge(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.full_combine(bin, *second);
        self
    }

    fn place(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.combine(bin, (*second).tree);
        self
    }

    fn left_merge(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.full_left_combine(bin, *second);
        self
    }

    fn left_place(&mut self, bin: PropBinary, second: Self) -> &mut Self {
        self.left_combine(bin, (*second).tree);
        self
    }

    fn go_up(&mut self) -> &mut Self {
        self.up_zip();
        self
    }

    fn go_right(&mut self) -> &mut Self {
        self.right_unzip();
        self
    }

    fn go_left(&mut self) -> &mut Self {
        self.left_unzip();
        self
    }

    fn go_down(&mut self) -> &mut Self {
        self.down_unzip();
        self
    }

    fn turn_right(&mut self) -> &mut Self {
        self.rotate_right();
        self
    }

    fn turn_left(&mut self) -> &mut Self {
        self.rotate_left();
        self
    }

    fn instance(&mut self, atoms: HashMap<Atom, Self>) -> &mut Self {
        let trees: HashMap<Atom, Tree<PropBinary, PropUnary, Atom>> =
            atoms.iter().map(|(&k, &v)| (k, v.tree)).collect();
        self.inorder_traverse_mut(&mut |f: &mut Formula<_, _, _>| f.instantiate(&trees));
        self
    }

    fn consume_unary(&mut self, unary: PropUnary) -> &mut Self {
        self.unify(unary);
        self
    }

    fn append_unary(&mut self, unary: PropUnary) -> &mut Self {
        self.append(unary);
        self
    }

    fn __str__(&self) -> String {
        self.to_string()
    }

    #[staticmethod]
    fn read(s: &str) -> PyResult<Self> {
        Ok(PropFormula::from_str(s)?.into())
    }
}
