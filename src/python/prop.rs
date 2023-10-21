use crate::Formula;
use pyo3::exceptions::PyValueError;
use pyo3::PyErr;

impl PyErrArguments for ParseError {
    fn arguments(self, py: pyo3::Python<'_>) -> pyo3::PyObject {
        self.to_string().into_py(py)
    }
}

impl From<ParseError> for PyErr {
    fn from(value: ParseError) -> Self {
        PyValueError::new_err(value)
    }
}

#[pyclass]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug, Default)]
struct PyAtom(pub usize);

impl Deref for PyAtom {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for PyAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if **self < ATOMS.len() {
            write!(f, "{}", ATOMS[**self])
        } else {
            write!(f, "{}", self.to_string())
        }
    }
}

impl Symbolic for PyAtom {}

impl Match for PyAtom {
    fn get_match(s: &str) -> Option<Self> {
        if let Some(i) = ATOMS.iter().position(|val| &s == val) {
            Some(PyAtom(i))
        } else if let Ok(i) = s.parse::<usize>() {
            Some(PyAtom(i))
        } else {
            None
        }
    }
}

#[pyclass]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
pub enum PyPropUnary {
    #[default]
    Not,
}

impl Display for PyPropUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PyPropUnary::Not => write!(f, "¬"),
        }
    }
}

impl Symbolic for PyPropUnary {}

impl Match for PyPropUnary {
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
#[pyclass]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
pub enum PyPropBinary {
    Iff,
    #[default]
    Implies,
    Or,
    And,
}

impl Display for PyPropBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PyPropBinary::Iff => write!(f, " ↔ "),
            PyPropBinary::Implies => write!(f, " → "),
            PyPropBinary::And => write!(f, " ∧ "),
            PyPropBinary::Or => write!(f, " ∨ "),
        }
    }
}

impl Symbolic for PyPropBinary {}

impl Match for PyPropBinary {
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
    fn from(value: PropFormula) -> Self {
        Proposition { formula: value }
    }
}

/// These are wrappers around the generic `Formula` methods which allow
/// you to use them in Python. Should probably macro this at some point!
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

    pub fn top_combine(&mut self, bin: PropBinary, second: Self) {
        self.deref_mut().top_combine(bin, second.formula);
    }

    pub fn top_left_combine(&mut self, bin: PropBinary, second: Self) {
        self.deref_mut().top_left_combine(bin, second.formula);
    }

    pub fn combine(&mut self, bin: PropBinary, second: Self) {
        self.deref_mut().combine(bin, second.formula.tree);
    }

    pub fn left_combine(&mut self, bin: PropBinary, second: Self) {
        self.deref_mut().left_combine(bin, second.formula.tree);
    }

    pub fn instance(&mut self, atoms: HashMap<Atom, Self>) {
        let trees: HashMap<Atom, Tree<PropBinary, PropUnary, Atom>> = atoms
            .into_iter()
            .map(|(k, v)| (k, v.formula.tree))
            .collect();
        self.inorder_traverse_mut(&mut |f: &mut Formula<_, _, _>| f.instantiate(&trees));
    }

    pub fn top_unify(&mut self, un: PropUnary) {
        self.deref_mut().top_unify(un)
    }

    pub fn unify(&mut self, un: PropUnary) {
        self.deref_mut().unify(un)
    }

    pub fn zip_up(&mut self) {
        self.deref_mut().zip_up()
    }

    pub fn zip_right(&mut self) {
        self.deref_mut().zip_right()
    }

    pub fn zip_left(&mut self) {
        self.deref_mut().zip_left()
    }

    pub fn zip(&mut self) {
        self.deref_mut().zip()
    }

    pub fn top_zip(&mut self) {
        self.deref_mut().top_zip()
    }

    pub fn unzip_down(&mut self) {
        self.deref_mut().unzip_down()
    }

    pub fn unzip_right(&mut self) {
        self.deref_mut().unzip_right()
    }

    pub fn unzip_left(&mut self) {
        self.deref_mut().unzip_left()
    }

    pub fn rotate_right(&mut self) {
        self.deref_mut().rotate_right()
    }

    pub fn rotate_left(&mut self) {
        self.deref_mut().rotate_left()
    }

    pub fn distribute_right(&mut self) {
        self.deref_mut().distribute_right()
    }

    pub fn distribute_left(&mut self) {
        self.deref_mut().distribute_left()
    }

    pub fn distribute_down(&mut self, new_bin: Option<PropBinary>) {
        self.deref_mut().distribute_down(new_bin)
    }

    pub fn lower_left(&mut self) {
        self.deref_mut().lower_left()
    }

    pub fn lower_right(&mut self) {
        self.deref_mut().lower_right()
    }

    pub fn push_down(&mut self, new_un: Option<PropUnary>) {
        self.deref_mut().push_down(new_un)
    }

    pub fn flip(&mut self) {
        self.deref_mut().flip()
    }

    pub fn __str__(&self) -> String {
        self.to_string()
    }

    pub fn __repr__(&self) -> String {
        self.to_string()
    }

    #[staticmethod]
    pub fn from_str(s: &str) -> PyResult<Self> {
        Ok(PropFormula::from_str(s)?.into())
    }
}
