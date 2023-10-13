use super::formula::{Tree, Zipper};
use pyo3::exceptions::PyValueError;
use pyo3::{pyclass, IntoPy, PyErr, PyErrArguments};
use std::fmt::Display;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

/// marker trait to show a type implements appropriate traits to be a symbol in a formula
pub trait Symbolic:
    Copy + PartialEq + Eq + PartialOrd + Ord + Clone + Display + Hash + Default
{
}

/// marker trait for symbol types to show they're parsable from strings
pub trait Parsable: Symbolic + Match {}

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
    /// Treating a sequence of symbols as an inorder-traversal representation of
    /// a formula tree, see if the current symbol comes at an appropriate place
    /// in the sequence. Unfortunately this method doesn't currently enjoy
    /// an efficient way of validating parentheses.
    fn validate_sequence(&self, seq: &[Self]) -> Result<&Self, ParseError> {
        match self {
            Symbol::Binary(_) => match seq.last() {
                None | Some(Symbol::Left) => Err(ParseError::IncompleteBinary),
                _ => Ok(self),
            },
            Symbol::Unary(_) => match seq.last() {
                Some(Symbol::Right) | Some(Symbol::Atom(_)) => Err(ParseError::DisconnectedClauses),
                _ => Ok(self),
            },
            Symbol::Atom(_) => match seq.last() {
                Some(Symbol::Atom(_)) | Some(Symbol::Right) => Err(ParseError::DisconnectedClauses),
                _ => Ok(self),
            },
            Symbol::Left | Symbol::Right => Ok(self),
        }
    }

    /// In a slice of symbols, find the lowest precedence operator that's not in parentheses.
    /// That means that a formula wrapped in parentheses returns an error.
    pub fn lowest_precedence(symbols: &[Self]) -> Result<(usize, Self), ParseError> {
        let mut symbol: Option<(usize, Self)> = None;
        let mut depth: isize = 0;
        for (i, sym) in symbols.iter().enumerate() {
            match sym {
                Symbol::Left => depth += 1,
                Symbol::Right => depth -= 1,
                _ => {
                    if let Some((i, s)) = symbol {
                        if s > *sym {
                            symbol = Some((i, *sym))
                        }
                    } else {
                        symbol = Some((i, *sym))
                    }
                }
            }
            if depth < 0 {
                return Err(ParseError::UnbalancedParentheses);
            }
        }
        if let Some(s) = symbol {
            Ok(s)
        } else {
            Err(ParseError::EmptyFormula)
        }
    }
}

impl<B, U, A> Match for Symbol<B, U, A>
where
    B: Parsable,
    U: Parsable,
    A: Parsable,
{
    fn get_match(s: &str) -> Option<Self> {
        if s == "(" {
            Some(Symbol::Left)
        } else if s == ")" {
            Some(Symbol::Right)
        } else if let Some(b) = B::get_match(s) {
            Some(Symbol::Binary(b))
        } else if let Some(u) = U::get_match(s) {
            Some(Symbol::Unary(u))
        } else if let Some(a) = A::get_match(s) {
            Some(Symbol::Atom(a))
        } else {
            None
        }
    }
}

pub trait Match: Sized {
    /// A trait that, when implemented for a type T, implements a method that, given a string,
    /// outputs a matching element of T if applicable.
    /// Also, whitespace and strings starting with whitespace
    /// can never be a match, as starting whitespace is always ignored by the parser.
    fn get_match(s: &str) -> Option<Self>;

    /// Match a prefix of a given string against the string matches. Uses the conventional
    /// max-munch principle: if the string is `"orange"` and `"o"` and `"or"` are both matches,
    /// the method will return `"or"`.
    fn match_prefix(s: &str) -> Option<(usize, Self)> {
        s.trim_start().char_indices().rev().find_map(|(i, _)| {
            if let Some(val) = Self::get_match(&s[..=i]) {
                Some((i, val))
            } else {
                None
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ParseError {
    InvalidChar(char),
    UnbalancedParentheses,
    IncompleteBinary,
    DisconnectedClauses,
    EmptyFormula,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidChar(c) => {
                write!(f, "{} does not correspond to a valid symbol.", c)
            }
            ParseError::UnbalancedParentheses => {
                write!(
                    f,
                    "The given string does not contain valid balanced parentheses."
                )
            }
            ParseError::IncompleteBinary => {
                write!(f, "A binary operator does not have two operands e.g. has a left parenthesis as an operand.")
            }
            ParseError::DisconnectedClauses => {
                write!(f, "A unary operator, atom, or empty space cannot connect two atomic clauses (i.e. either a parenthesized clause or an atom).")
            }
            ParseError::EmptyFormula => {
                write!(f, "Empty formula is not valid.")
            }
        }
    }
}

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

pub struct ParsedSymbols<B, U, A>(pub Result<Vec<Symbol<B, U, A>>, ParseError>)
where
    B: Parsable,
    U: Parsable,
    A: Parsable;

impl<B, U, A> Deref for ParsedSymbols<B, U, A>
where
    B: Parsable,
    U: Parsable,
    A: Parsable,
{
    type Target = Result<Vec<Symbol<B, U, A>>, ParseError>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<B, U, A> DerefMut for ParsedSymbols<B, U, A>
where
    B: Parsable,
    U: Parsable,
    A: Parsable,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<B, U, A> From<&str> for ParsedSymbols<B, U, A>
where
    B: Parsable,
    U: Parsable,
    A: Parsable,
{
    fn from(value: &str) -> Self {
        let (mut left, mut right): (usize, usize) = (0, 0);
        let mut start: usize = 0;
        let mut syms: Vec<Symbol<B, U, A>> = Vec::new();
        while let Some((i, sym)) = Symbol::match_prefix(&value[start..]) {
            if let Err(e) = sym.validate_sequence(&syms[..]) {
                return ParsedSymbols(Err(e));
            }
            if let Symbol::Left = sym {
                left += 1
            } else if let Symbol::Right = sym {
                right += 1
            }
            if right >= left {
                return ParsedSymbols(Err(ParseError::UnbalancedParentheses));
            }
            syms.push(sym);
            start += i;
        }
        if start != value.len() {
            ParsedSymbols(Err(ParseError::InvalidChar(
                value[start..].chars().next().unwrap_or('\0'),
            )))
        } else {
            ParsedSymbols(Ok(syms))
        }
    }
}

static ATOMS: [&'static str; 52] = [
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
#[pyclass]
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
    fn get_match(s: &str) -> Option<Self> {
        if let Some(i) = ATOMS.iter().position(|val| &s == val) {
            Some(Atom(i))
        } else if let Ok(i) = s.parse::<usize>() {
            Some(Atom(i))
        } else {
            None
        }
    }
}

impl Parsable for Atom {}
