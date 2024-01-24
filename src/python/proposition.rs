use crate::formula::{Formula, Tree, Zipper};
use crate::parser::{build_formula, Match, ParseError, ParsedSymbols};
use crate::prop::{PropBinary, PropFormula, PropSymbol, PropUnary};
use crate::symbol::{Atom, Symbolic};
use enum_iterator::*;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::PyErr;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};

#[pymethods]
impl Atom {
    #[new]
    #[pyo3(signature = (num=0, s=None))]
    fn new(num: usize, s: Option<&str>) -> PyResult<Self> {
        if let Some(string) = s {
            Atom::match_str(string).ok_or(ParseError::EmptyFormula.into())
        } else {
            Ok(Atom(num))
        }
    }

    #[getter]
    fn get_value(&self) -> usize {
        self.0
    }

    #[setter]
    fn set_value(&mut self, new_val: usize) {
        self.0 = new_val
    }

    fn __str__(&self) -> String {
        self.to_string()
    }
    fn __repr__(&self) -> String {
        self.to_string()
    }

    fn str_matches(&self) -> Vec<String> {
        self.get_matches()
    }

    #[staticmethod]
    fn try_match(s: &str) -> Option<Self> {
        Self::match_str(s)
    }
}

#[pymethods]
impl PropBinary {
    fn str_matches(&self) -> Vec<String> {
        self.get_matches()
    }

    #[staticmethod]
    fn try_match(s: &str) -> Option<Self> {
        Self::match_str(s)
    }

    #[staticmethod]
    /// How many of these operators there are!
    fn size() -> usize {
        enum_iterator::cardinality::<PropBinary>()
    }

    #[staticmethod]
    /// Offer an indexing map for all operators.
    fn counting(offset: usize) -> HashMap<PropBinary, usize> {
        all::<PropBinary>()
            .map(|s| (s, s as usize + offset))
            .collect::<HashMap<PropBinary, usize>>()
    }

    #[staticmethod]
    /// Offer an indexing map for the string repr of the operators.
    fn str_counting(offset: usize) -> HashMap<String, usize> {
        all::<PropBinary>()
            .map(|s| (s.to_string(), s as usize + offset))
            .collect::<HashMap<String, usize>>()
    }
}

#[pymethods]
impl PropUnary {
    fn str_matches(&self) -> Vec<String> {
        self.get_matches()
    }

    #[staticmethod]
    fn try_match(s: &str) -> Option<Self> {
        Self::match_str(s)
    }
    #[staticmethod]
    /// How many of these operators there are!
    fn size() -> usize {
        enum_iterator::cardinality::<PropUnary>()
    }

    #[staticmethod]
    /// Offer an indexing map for all operators.
    fn counting(offset: usize) -> HashMap<PropUnary, usize> {
        all::<PropUnary>()
            .map(|s| (s, s as usize + offset))
            .collect::<HashMap<PropUnary, usize>>()
    }

    #[staticmethod]
    /// Offer an indexing map for the string repr of the operators.
    fn str_counting(offset: usize) -> HashMap<String, usize> {
        all::<PropUnary>()
            .map(|s| (s.to_string(), s as usize + offset))
            .collect::<HashMap<String, usize>>()
    }
}

/// The Python-bound instance of formula for propositional formulas.
/// This language includes the negation operator and operators for
/// or, and, implication and the biconditional.
#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug)]
#[pyclass]
pub struct Proposition {
    formula: PropFormula,
}

impl std::ops::Deref for Proposition {
    type Target = PropFormula;

    fn deref(&self) -> &Self::Target {
        &self.formula
    }
}

impl std::ops::DerefMut for Proposition {
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
    pub fn new(s: &str) -> PyResult<Self> {
        Ok(build_formula::<PropBinary, PropUnary, Atom>(s)?.into())
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
        self.inorder_traverse_mut(&mut |f: &mut Formula<_, _, _>| Some(f.instantiate(&trees)));
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

    /// Because we can't access the generic [`crate::symbol::Symbol`] type
    /// in Python we'll take in a map of strings to nonnegative integers and
    /// then use [`crate::parser::Match`] to map them to the symbols.
    pub fn tensorize(
        &mut self,
        mapping: HashMap<&str, usize>,
    ) -> PyResult<(Vec<usize>, Vec<Vec<usize>>)> {
        Ok(self.deref_mut().tensorize(
            &mapping
                .into_iter()
                .map_while(|(s, i)| Some((PropSymbol::match_str(s)?, i)))
                .collect::<HashMap<PropSymbol, usize>>(),
        )?)
    }

    pub fn get_atoms(&self) -> HashSet<Atom> {
        self.deref().get_atoms()
    }

    pub fn normalize(&self, indices: Vec<Atom>) -> Option<Self> {
        Some(self.deref().normalize(indices.into_iter())?.into())
    }

    pub fn __str__(&self) -> String {
        self.to_string()
    }

    pub fn __repr__(&self) -> String {
        self.to_string()
    }

    #[staticmethod]
    pub fn from_str(s: &str) -> PyResult<Self> {
        Ok(build_formula::<PropBinary, PropUnary, Atom>(s)?.into())
    }
}
