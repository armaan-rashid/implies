//! *implies* is a Rust crate for easily creating and manipulating
//! logical formulas represented as parse trees.
//!
//! A few things make using *implies* more ergonomic than, say, a generic
//! binary tree data structure, for a logic use-case.
//!
//! 1. The [`Formula`] struct, which is the heart of the crate, is an `enum`
//!    which makes it impossible to construct ill-formed formulas by forcing each
//!    node to specify whether it's an atomic formula, a unary formula, or a binary formula.
//! 2. [`Formula`] is generic over any language which has binary and unary operators,
//!    and atomic formulas, i.e. the overwhelming majority of logics.
//!    Simply providing enums for your operators and atomic types
//!    grants you access to the struct's full power.
//! 3. Transformations are safe and fast. Internally the formulas are implemented as
//!    a kind of [`Zipper`] which allows easy in-place mutation. All transforming methods
//!    are designed for safety, only being executed when the state of the formula is valid
//!    for that specific transformation. This also allows code that can call large chains
//!    of transformations on formulae without branching, often necessary when executing common
//!    transformation algorithms, e.g. natural deduction rules or conversion to normal forms.
//! 4. The [`Formula`] struct comes with several transformations that are ubiquitous for logical
//!    formulas like rotating precedence and operator distribution.
//! 5. If you implement the [`Match`] trait for your types, you have full access to a lexer and parser
//!    which converts strings into formulas for you, making it easy to convert back and forth between
//!    strings and trees for your own formulas.
//!
//! There is a built-in implementation of propositional logic as [`PropFormula`], but again,
//! you can immediately start using this crate by defining your own atomic, binary operator, and
//! unary operator types for whatever logic you want to use. They do have to implement the [`Symbolic`]
//! trait which is just a wrapper of pretty basic requirements for the most part. The surprising ones
//! may be [`Ord`] + [`PartialOrd`], but this is of course for determining operator precedence; and
//! [`Copy`] which is because it's assumed the types of the operators/atoms are cheap, i.e. fieldless enums
//! or integers. This may be relaxed later, however.
//!
//! Fair warning, though: if you've never used a [`Zipper`] data structure before e.g. in a functional
//! language, this crate may seem really counterintuitive. It's recommended to understand/play around
//! with zipper data structures a little bit before intensely using this crate.
//!
//! [`PropFormula`]: self::prop::PropFormula
//! [`Symbolic`]: self::symbol::Symbolic
//! [`Ord`]: std::cmp::Ord
//! [`PartialOrd`]: std::cmp::PartialOrd
//! [`Copy`]: std::marker::Copy
//! [`Formula`]: formula::Formula
//! [`Zipper`]: formula::Zipper
//! [`Match`]: parser::Match

pub mod formula;
pub mod modal;
pub mod parser;
pub mod prop;
pub mod symbol;

mod tests;

#[cfg(feature = "python")]
pub mod python;

#[cfg(feature = "python")]
use {pyo3::prelude::*, python::proposition::Proposition};

#[cfg(feature = "python")]
#[pymodule]
fn implies(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<symbol::Atom>()?;
    m.add_class::<prop::PropBinary>()?;
    m.add_class::<prop::PropUnary>()?;
    m.add_class::<Proposition>()?;
    Ok(())
}
