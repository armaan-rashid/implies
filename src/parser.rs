use crate::formula::{Formula, Tree};
use crate::symbol::{Symbol, Symbolic};
use std::fmt::Display;
use std::ops::{Deref, DerefMut};

/// A trait that, when implemented for a type T, implements a method that, given a string,
/// outputs a matching element of T if applicable.
/// Also, whitespace and strings starting with whitespace
/// can never be a match, as starting whitespace is always ignored by the parser.
pub trait Match: Sized {
    /// Given a string, return Self if the string matches
    /// some element of Self.
    fn match_str(s: &str) -> Option<Self>;

    /// Given an element of Self, return the corresponding
    /// strings as a vector.
    fn get_matches(&self) -> Vec<String>;

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
                Self::match_str(&s[..last_char + char_width].trim_start())?,
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
    IncompleteEnum,
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
            ParseError::IncompleteEnum => {
                write!(f, "When attempting to convert the formula to a tensor an incomplete mapping from symbols to node features was provided.")
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

/// A utility for stripping outer parentheses from a slice of symbols if applicable.
/// This also catches unbalanced parentheses in a slice.
pub fn strip_parentheses<B, U, A>(
    syms: &[Symbol<B, U, A>],
) -> Result<&[Symbol<B, U, A>], ParseError>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
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
/// [`strip_parentheses`]: self::strip_parentheses
pub fn main_operator<B, U, A>(
    symbols: &[Symbol<B, U, A>],
) -> Result<(usize, Symbol<B, U, A>), ParseError>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    let mut symbol: Option<(usize, Symbol<B, U, A>)> = None;
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

/// Recursively build a tree from a slice of symbols.
pub fn build_tree<B, U, A>(syms: &[Symbol<B, U, A>]) -> Result<Tree<B, U, A>, ParseError>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    let symbols = strip_parentheses(syms)?;
    match main_operator(symbols)? {
        (i, Symbol::Binary(b)) => Ok(Tree::Binary {
            conn: b,
            left: Box::new(build_tree(&symbols[..i])?),
            right: Box::new(build_tree(&symbols[i + 1..])?),
        }),
        (i, Symbol::Unary(u)) => Ok(Tree::Unary {
            conn: u,
            next: Box::new(build_tree(&symbols[i + 1..])?),
        }),
        (_, Symbol::Atom(a)) => Ok(Tree::Atom(a)),
        _ => unreachable!(), // main_operator never returns a parenthesis
    }
}

/// As expected, read a formula from a string. Return error if the string is malformed.
pub fn build_formula<B: Symbolic + Match, U: Symbolic + Match, A: Symbolic + Match>(
    value: &str,
) -> Result<Formula<B, U, A>, ParseError> {
    Ok(build_tree(&ParsedSymbols::from(value).0?[..])?.into())
}
