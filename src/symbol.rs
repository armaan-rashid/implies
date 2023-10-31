//! This module contains the generic [`crate::symbol::Symbol`] type which wraps atomic,
//! binary and unary operator types, and also other types that are likely to be generic over
//! multiple logical languages, such as [`crate::symbol::Atom`] which can be used as an atomic
//! type for many different logics.

use super::formula::{Tree, Zipper};
use crate::parser::Match;
#[cfg(feature = "python")]
use pyo3::prelude::*;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Deref;

/// marker trait to show a type implements appropriate traits to be a symbol in a formula
pub trait Symbolic:
    Copy + PartialEq + Eq + PartialOrd + Ord + Clone + Display + Hash + Default
{
}

#[derive(Copy, PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum Symbol<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    Binary(B),
    Unary(U),
    Atom(A),
    Left,
    Right, // Left and Right parentheses
}

impl<B, U, A> Display for Symbol<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Binary(x) => {
                write!(f, "{}", x.to_string())
            }
            Symbol::Unary(x) => {
                write!(f, "{}", x.to_string())
            }
            Symbol::Atom(x) => {
                write!(f, "{}", x.to_string())
            }
            Symbol::Left => {
                write!(f, "(")
            }
            Symbol::Right => {
                write!(f, ")")
            }
        }
    }
}

/// A generic type for when we need to compare over B, U, and A, the types
/// that go into our formulae. Since they implement Ord individually this wrapper
/// type allows comparison between any of the three types assuming the convention
/// that U(nary) operators always have higher precedence than B(inary) operators.
impl<B, U, A> Symbol<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    pub fn from_tree(t: &Tree<B, U, A>) -> Self {
        match t {
            Tree::Binary {
                conn,
                left: _,
                right: _,
            } => Symbol::Binary(*conn),
            Tree::Unary { conn, next: _ } => Symbol::Unary(*conn),
            Tree::Atom(a) => Symbol::Atom(*a),
        }
    }

    /// Turn the 'value' of a zipper into a symbol, or none for a top zipper.
    pub fn from_zipper(z: &Zipper<B, U, A>) -> Option<Self> {
        match z {
            Zipper::Top => None,
            Zipper::Right { bin, .. } => Some(Symbol::Binary(*bin)),
            Zipper::Left { bin, .. } => Some(Symbol::Binary(*bin)),
            Zipper::Up { un, .. } => Some(Symbol::Unary(*un)),
        }
    }
}

impl<B, U, A> Match for Symbol<B, U, A>
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match,
{
    fn match_str(s: &str) -> Option<Self> {
        if s == "(" {
            Some(Symbol::Left)
        } else if s == ")" {
            Some(Symbol::Right)
        } else if let Some(b) = B::match_str(s) {
            Some(Symbol::Binary(b))
        } else if let Some(u) = U::match_str(s) {
            Some(Symbol::Unary(u))
        } else if let Some(a) = A::match_str(s) {
            Some(Symbol::Atom(a))
        } else {
            None
        }
    }

    fn get_matches(&self) -> Vec<String> {
        match self {
            Symbol::Binary(s) => s.get_matches(),
            Symbol::Unary(s) => s.get_matches(),
            Symbol::Atom(s) => s.get_matches(),
            Symbol::Left => vec!["(".to_string()],
            Symbol::Right => vec![")".to_string()],
        }
    }
}

pub static ATOMS: [&'static str; 52] = [
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s",
    "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
    "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
];

/// A simple type to represent atoms: a wrapper around unsigned integers.
/// Implements Deref to `usize` for ease of use. In terms of being parsed,
/// any atom less than 26 maps to a corresponding lowercase letter and those
/// from `26..52` map to the corresponding uppercase letter. If for whatever
/// reason you need more than 52 atoms, then they can only be printed/parsed
/// as the corresponding numbers.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug, Default)]
#[cfg_attr(feature = "python", pyclass)]
pub struct Atom(pub usize);

impl Deref for Atom {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if **self < ATOMS.len() {
            write!(f, "{}", ATOMS[**self])
        } else {
            write!(f, "{}", self.to_string())
        }
    }
}

impl Symbolic for Atom {}

impl Match for Atom {
    fn match_str(s: &str) -> Option<Self> {
        if let Some(i) = ATOMS.iter().position(|val| &s == val) {
            Some(Atom(i))
        } else if let Ok(i) = s.parse::<usize>() {
            Some(Atom(i))
        } else {
            None
        }
    }

    fn get_matches(&self) -> Vec<String> {
        match self.0 {
            ..=51 => vec![self.0.to_string(), ATOMS[self.0].to_string()],
            _ => vec![self.0.to_string()],
        }
    }
}
