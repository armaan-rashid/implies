use super::formula::{Tree, Zipper};
#[cfg(feature = "python")]
use pyo3::pyclass;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

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

    /// A utility for stripping outer parentheses from a slice of symbols if applicable.
    /// This also catches unbalanced parentheses in a slice.
    pub fn strip_parentheses(syms: &[Self]) -> Result<&[Self], ParseError> {
        if syms.is_empty() {
            return Ok(syms);
        }
        let mut outer: usize = 0;
        while let (Symbol::Left, Symbol::Right) = (syms[outer], syms[syms.len() - outer - 1]) {
            outer += 1;
        }
        let (mut left, mut right, mut start) = (0 as usize, 0 as usize, outer);
        for s in &syms[outer..syms.len() - outer] {
            if let Symbol::Left = s {
                left += 1
            } else if let Symbol::Right = s {
                right += 1
            }
            if right > left + outer {
                break; // unbalanced!
            } else if right > left && start > 0 {
                start -= 1 // now, a left paren is "for" the right paren
            }
        }
        if left != right {
            Err(ParseError::UnbalancedParentheses)
        } else {
            Ok(&syms[start..syms.len() - start])
        }
    }

    /// In a slice of logical symbols, find the lowest precedence operator, i.e. the main
    /// operator that's not in parentheses. Also basically validates the slice of symbols.
    /// Parentheses are treated as black boxes, so if the whole formula is wrapped in parentheses
    /// it may be valid but this method will return an error! [`strip_parentheses`] first.
    ///
    /// [`strip_parentheses`]: Symbol::strip_parentheses
    pub fn main_operator(symbols: &[Self]) -> Result<(usize, Self), ParseError> {
        let mut symbol: Option<(usize, Self)> = None;
        let mut depth: isize = 0;
        for (i, sym) in symbols.iter().enumerate() {
            match sym {
                Symbol::Left => depth += 1,
                Symbol::Right => depth -= 1,
                _ => {
                    if depth == 0 && (symbol.is_none() || sym < &symbol.unwrap().1) {
                        symbol = Some((i, *sym))
                    }
                }
            }
        }
        match symbol {
            Some((_, Symbol::Binary(_))) | Some((0, Symbol::Unary(_))) => Ok(symbol.unwrap()),
            Some((i, Symbol::Unary(_))) => Err(ParseError::UnaryLeft(symbols[i - 1].to_string())),
            Some((i, Symbol::Atom(a))) => {
                if symbols.len() != 1 {
                    Err(ParseError::NotAtomic(symbols[1].to_string()))
                } else {
                    Ok((i, Symbol::Atom(a)))
                }
            }
            None => Err(ParseError::EmptyFormula),
            _ => unreachable!(),
        }
    }
}

impl<B, U, A> Match for Symbol<B, U, A>
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match,
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
        // the ugliness of calculating the width is only because
        // the char rounding APIs are still in nightly
        let mut last_char: usize = s.len();
        s.char_indices().rev().find_map(|(i, _)| {
            let char_width = last_char - i;
            last_char = i;
            Some((
                last_char + char_width,
                Self::get_match(&s[..last_char + char_width].trim_start())?,
            ))
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    InvalidStr(String),
    UnbalancedParentheses,
    NotAtomic(String),
    UnaryLeft(String),
    EmptyFormula,
}

impl std::error::Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidStr(s) => {
                write!(f, "{} does not correspond to a valid symbol.", s)
            }
            ParseError::UnbalancedParentheses => {
                write!(f, "The string does not contain valid balanced parentheses.")
            }
            ParseError::NotAtomic(s) => {
                write!(f, "The symbol {} is next to what should be a lone atom.", s)
            }
            ParseError::UnaryLeft(s) => {
                write!(f, "Some {} precedes a unary operator that shouldn't.", s)
            }
            ParseError::EmptyFormula => {
                write!(f, "The empty formula is not valid. This error often occurs if a binary and/or unary operator are not given proper operands.")
            }
        }
    }
}

pub struct ParsedSymbols<B, U, A>(pub Result<Vec<Symbol<B, U, A>>, ParseError>)
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match;

impl<B, U, A> Deref for ParsedSymbols<B, U, A>
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match,
{
    type Target = Result<Vec<Symbol<B, U, A>>, ParseError>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<B, U, A> DerefMut for ParsedSymbols<B, U, A>
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<B, U, A> From<&str> for ParsedSymbols<B, U, A>
where
    B: Symbolic + Match,
    U: Symbolic + Match,
    A: Symbolic + Match,
{
    fn from(value: &str) -> Self {
        let mut start: usize = 0;
        let mut syms: Vec<Symbol<B, U, A>> = Vec::new();
        while let Some((width, sym)) = Symbol::match_prefix(&value[start..]) {
            syms.push(sym);
            start += width;
        }
        if !value[start..].trim_start().is_empty() {
            ParsedSymbols(Err(ParseError::InvalidStr(value[start..].to_string())))
        } else {
            ParsedSymbols(Ok(syms))
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
