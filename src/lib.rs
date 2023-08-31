use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use std::fmt::{Display, Debug};
use std::collections::{HashSet, HashMap};
use std::hash::Hash;
use rand::prelude::*;
use rand::rngs::StdRng;

static NUM_ATOMS: usize = 26;
static ATOMS: [char; 26] = [
    'A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'
];

// static ID: fn(PropFormula) -> PropFormula = |f: PropFormula| f;

#[derive(Debug, Clone)]
pub enum SymbolParseError {
    InvalidChar(char),
    UnbalancedParentheses,
}

impl Display for SymbolParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolParseError::InvalidChar(c) => {write!(f, "{} does not correspond to a valid symbol.", c.to_string())}
            SymbolParseError::UnbalancedParentheses => {write!(f, "The given string does not contain valid balanced parentheses.")},
        }
    }
}

impl std::error::Error for SymbolParseError {}



#[derive(Debug)]
pub enum ValidationError {
    Binary(BinaryOp),
    Unary(UnaryOp),
    Atom(char),
    Parentheses,
    EmptyFormula,
    NoLiteral,
    NoConnectives,
    MismatchedSymbol(Symbol),
    ParseError(SymbolParseError)
}

impl Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::Binary(b) => {write!(f, "Binary operator {} doesn't have two children.", b.to_string())},
            ValidationError::Unary(u) => {write!(f, "Unary operator {} doesn't have right child, has left child, or both.", u.to_string())},
            ValidationError::Atom(a) => {write!(f, "Atom {} has children.", a.to_string())},
            ValidationError::Parentheses => {write!(f, "No parentheses in a tree formula!")},
            ValidationError::EmptyFormula => {write!(f, "The empty formula is not constructible.")},
            ValidationError::ParseError(spe) => write!(f, "{}", spe.to_string()),
            ValidationError::NoLiteral => {write!(f, "All subformulas need to terminate in a literal.")},
            ValidationError::NoConnectives => {write!(f, "This formula doesn't have any connectives, but has more than one literal.")},
            ValidationError::MismatchedSymbol(s) => {write!(f, "{} is not the expected symbol at this point in the formula.", s.to_string())}
        }
    }
}

impl std::error::Error for ValidationError {}

impl From<ValidationError> for PyErr {
    fn from(value: ValidationError) -> Self {
        PyValueError::new_err(value.to_string())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum UnaryOp {
    Not
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "¬"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum BinaryOp {
    Iff,
    Implies, 
    Or, 
    And, 
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Iff => write!(f, " ↔ "),
            BinaryOp::Implies => write!(f, " → "), 
            BinaryOp::And => write!(f, " ∧ "),
            BinaryOp::Or => write!(f, " ∨ "),    
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Binary(BinaryOp),
    Unary(UnaryOp),
    Atom(char),
    Left,   // i.e. left and right parentheses
    Right,
}

impl Display for Symbol {
    /// so we can have some nicely printed PropFormulas!
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Left => write!(f, "("),
            Symbol::Right => write!(f, ")"),
            Symbol::Unary(unary) => write!(f, "{}", unary.to_string()),
            Symbol::Binary(binary) => write!(f, "{}", binary.to_string()),
            Symbol::Atom(s) => write!(f, "{}", s.to_string()),
        }
    }
}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            

/// Some negative pattern matching utilities for use in other code where needed,
/// since it's not built into Rust syntax currently.
impl Symbol {
    fn not_binary(&self) -> bool {
        if let Symbol::Binary(_) = *self {false}
        else {true}
    }

    fn not_unary(&self) -> bool {
        if let Symbol::Unary(_) = *self {false}
        else {true}
    }

    fn not_atom(&self) -> bool {
        if let Symbol::Atom(_) = *self {false}
        else {true}
    }

    fn to_integer(&self) -> usize {
        match self {
            Symbol::Binary(BinaryOp::Implies) => 53,
            Symbol::Unary(_) => 52,
            Symbol::Atom(a) => {
                if char::is_alphabetic(*a) {
                    *a as usize - 'A' as usize
                }
                else {54}
            },
            _ => 54,  // indicates an invalid embedding
        }
    }
} 

/// Stores a propositional logic formula as a binary tree: connectives 
/// at each node except the leaves which contain propositions.
#[derive(Clone, Hash, PartialOrd, Ord)]
#[pyclass]
pub struct PropFormula {
    pub root: Symbol,
    left: Option<Box<PropFormula>>,
    right: Option<Box<PropFormula>>,
    size: usize
}

impl Debug for PropFormula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}


impl PropFormula {
    /// This struct stores well-formed formulas in propositional logic as trees, but 
    /// allows for conversion to the 'normal' form too. The mutating methods assume
    /// that the formulas being mutated are valid, which can be checked with 
    /// the .validate() method.
    /// Crucially, this implementation treats propositional 'atoms' like they are also 
    /// possible instances of formulas themselves; in this sense the atoms (which are stored 
    /// as Strings) are used like placeholders, which can be replaced with other PropFormulas.
    /// This functionality is provided by the essential .instantiate() method.

    pub fn new(prop: char) -> Self {
        PropFormula {
            root: Symbol::Atom(prop),
            left: None,
            right: None,
            size: 1
        }
    }


    /// Does exactly what you think it does!
    pub fn random_atom(set: Option<&HashSet<char>>) -> PropFormula {
        if let Some(hs) = set {
            if !hs.is_empty() {
                let atom = hs
                    .iter()
                    .choose(&mut thread_rng())
                    .unwrap();
                return PropFormula::new(*atom);
            }
        }
        PropFormula::new(ATOMS[random::<usize>() % NUM_ATOMS])
    }


    fn traverse_atoms(&self, atoms: &mut HashSet<char>) {
        if let Symbol::Atom(a) = &self.root {
            atoms.insert(*a);
        }
        self.left.as_ref().map_or((), |f| f.traverse_atoms(atoms));
        self.right.as_ref().map_or((), |f| f.traverse_atoms(atoms));
    }

    /// Calling this on a PropFormula reassigns all its fields to that of the 'eaten'
    /// PropFormula.
    fn reassign(self, eaten: Self) -> Self {
        let mut formula = self;
        (formula.root, formula.left, formula.right, formula.size) = (eaten.root, eaten.left, eaten.right, eaten.size);
        formula
    }


    /// Replace an 'atom' with another formula.
    fn instantiate_atom(self, atomic_formulas: &HashMap<char, PropFormula>) -> Self {
        let mut formula = self;
        if let Symbol::Atom(a) = formula.root {
            if atomic_formulas.contains_key(&a) {formula = formula.reassign(atomic_formulas[&a].clone())}
        } formula
    }



    /// Recursive implementation of is_instance. Traverse two formulae at once to see if the
    /// formula this is called on is actually an instance of the second formula, with 
    /// the second formula's 'atoms' replaced by some other PropFormula.
    /// Importantly, a HashMap is used to make sure that atoms in the axiom are consistently
    /// mapped to the SAME subformula in the instance formula.
    fn match_traverse<'a>(&'a self, axiom: &PropFormula, atomic_formulas: &mut HashMap<char, &'a PropFormula>) -> bool {
        if let Symbol::Atom(a) = &axiom.root {      // base case: reached an atom, check for consistency w/ prior atom checks
            if atomic_formulas.contains_key(a) {self == atomic_formulas[a]}
            else {
                atomic_formulas.insert(a.clone(), self);
                true
            }
        } else {
            match (&self.left, &axiom.left, &self.right, &axiom.right) {
                (Some(_), Some(_), Some(_), Some(_)) => self.get_child(false)
                                                            .match_traverse(axiom.get_child(false), atomic_formulas)
                                                    &&  self.get_child(true)
                                                            .match_traverse(axiom.get_child(true), atomic_formulas),
                (Some(_), Some(_), None, None) => self.get_child(false)
                                                      .match_traverse(axiom.get_child(false), atomic_formulas),
                (None, None, Some(_), Some(_)) => self.get_child(true)
                                                      .match_traverse(axiom.get_child(true), atomic_formulas),

                _ => false
            }
        }
    }

    
    /// Implementation for to_inorder_sequence, could theoretically be used by users.
    fn preorder_traverse(&self, seq: &mut Vec<Symbol>) {
        seq.push(self.root);
        self.left.as_ref().map_or((), |f| f.preorder_traverse(seq));
        self.right.as_ref().map_or((), |f| f.preorder_traverse(seq));
    }
    
    fn to_preorder_sequence(&self) -> Vec<Symbol> {
        let mut seq: Vec<Symbol> = Vec::new();
        self.preorder_traverse(&mut seq);
        seq
    }
    
    /// Since default string is inorder traversal, this is for the preorder string representation if desired.
    fn to_preorder_string(&self) -> String {
        self.to_preorder_sequence()
            .iter()
            .map(|s| s.to_string())
            .collect::<String>()
        }
        
        
    /// Actual implementation for to_inorder_sequence, could be used directly.
    fn inorder_traverse(&self, seq: &mut Vec<Symbol>) {
        if let Some(prop_formula) = &self.left {
            if prop_formula.root <= self.root {seq.push(Symbol::Left)}     
            prop_formula.inorder_traverse(seq);
            if prop_formula.root <= self.root {seq.push(Symbol::Right)}
        }
        seq.push(self.root.clone());
        if let Some(prop_formula) = &self.right {
            if prop_formula.root < self.root {seq.push(Symbol::Left)}       // lack of equals sign bc we assume right-associativity
            prop_formula.inorder_traverse(seq);
            if prop_formula.root < self.root {seq.push(Symbol::Right)}
        }
    }
    
    /// Perform an inorder traversal to turn the PropFormula into a linear sequence.
    fn to_inorder_sequence(&self) -> Vec<Symbol> {
        let mut seq: Vec<Symbol> = Vec::new();
        self.inorder_traverse(&mut seq);
        seq
    }

    fn to_inorder_string(&self) -> String {
        self.to_inorder_sequence()
            .iter()
            .map(|s| s.to_string())
            .collect::<String>()
    }
    
    /// Validate that the formula as a tree is actually well-formed.
    /// i.e., unary operators only have right and not left children,
    /// binary operators have both operands, there's left and
    /// right operands for binary operators, and no parentheses in 
    /// the actual tree structure!
    pub fn validate(&self) -> Result<&Self, ValidationError> {
        match self.root {
            Symbol::Binary(b) => {
                if let (Some(_), Some(_)) = (&self.left, &self.right) {
                    let (_left, _right) = (self.get_child(false).validate()?, 
                                                   self.get_child(true).validate()?);
                    Ok(self)
                } else {Err(ValidationError::Binary(b))}
            },
            Symbol::Unary(u) => {
                if let (None, Some(_)) = (&self.left, &self.right) {
                    let _right = self.get_child(true).validate()?;
                    Ok(self)
                } else {Err(ValidationError::Unary(u))}
            },
            Symbol::Atom(a) => {
                if let (None, None) = (&self.left, &self.right) {Ok(self)}
                else {Err(ValidationError::Atom(a))}
            },
            Symbol::Left | Symbol::Right => Err(ValidationError::Parentheses),
        } 
    }
    
    /// Some syntax sugar to get a reference to right/left child without the Option<Box<_>> wrapper.
    /// Assumes the child actually exists, otherwise panics: deliberately does NOT return Option<PropFormula>.
    /// These funcs meant for situations where we want clean chaining during construction/evaluation and
    /// we know the child exists, without having to deal with changing return types of callers to Options
    /// themselves for the sake of the ? operator.
    pub fn get_child<'a>(&'a self, right: bool) -> &'a Self {
        if right {self.right.as_ref().unwrap()} else {self.left.as_ref().unwrap()}
    }

    /// Some syntax sugar to get a mutable reference to right/left child without the Option<Box<_>> wrapper.
    /// Assumes the child actually exists, otherwise panics!
    pub fn get_mut_child<'a>(&'a mut self, right: bool) -> &'a mut Self {
        if right {self.right.as_mut().unwrap()} else {self.left.as_mut().unwrap()}
    }

    /// Literally just take the kid outta there. The other child / the root is
    /// abandoned. Like get_child() methods this panics if there's no child there.
    pub fn take_child(self, right: bool) -> Self {
        if right {*self.right.unwrap()} else {*self.left.unwrap()}
    }

    /// Both children this time. Panics if there are no children there!
    pub fn take_children(self) -> (Self, Self) {
        (*self.right.unwrap(), *self.left.unwrap())
    }

    /// The same as the get_child() methods but when you actually want to do something to your kids.
    /// Creepy name for sure. Doesn't do anything if the children don't exist.
    pub fn possess_child<F>(self, right: bool, method: F) -> Self 
    where F: FnOnce(Self) -> Self
    {
        let mut formula = self;
        if right {if let Some(_) = formula.right {formula.right = Some(Box::new(method(*formula.right.unwrap())))}} 
        else {if let Some(_) = formula.left {formula.left = Some(Box::new(method(*formula.left.unwrap())))}}
        formula
    }

    /// Control both your kids.
    pub fn possess_children<F1, F2>(self, left_method: F1, right_method: F2) -> Self
    where F1: FnOnce(Self) -> Self, F2: FnOnce(Self) -> Self
    {
        if let Symbol::Binary(b) = self.root {
            let (left, right) = (*self.left.unwrap(), *self.right.unwrap());
            left_method(left).combine(b, right_method(right))
        } else {self}
    }


    /// 'Rotate' a tree formula, i.e. change precedence between two binary operators, 
    /// either to the left or right. As an example,
    ///     →                                       
    ///   /   \
    /// A       ∧          
    ///       /   \
    ///     B       C 
    /// 
    ///         =>
    /// 
    ///             ∧                               
    ///           /   \
    ///         →       C   
    ///       /   \
    ///     A       B 
    /// 
    /// is an example of a left rotation.
    fn rotate_left(self) -> Self {
        if let Some(_) = self.right {
            if let (Symbol::Binary(bin1), Symbol::Binary(bin2)) = (self.root, self.get_child(true).root) {
                let (a, bc) = self.take_children();
                let (b,c) = bc.take_children();
                a.combine(bin1, b).combine(bin2, c)
            } else {self}
        } else {self}
    }
    
    /// The exact same as .rotate_left() but to the right! Reversing
    /// the transformation given in the docstring for rotate_right().
    fn rotate_right(self) -> Self {
        if let Some(_) = self.right {
            if let (Symbol::Binary(bin2), Symbol::Binary(bin1)) = (self.root, self.get_child(false).root) {
                let (ab, c) = self.take_children();
                let (a, b) = ab.take_children();
                a.combine(bin1, b.combine(bin2, c))
            } else {self}
        } else {self}
    }

    /// Append the not operator to a formula,
    /// making a copy and returning the new version.
    /// The original formula is always the right child
    /// in the new formula.
    pub fn negate(self) -> Self {
        let sz = self.size;
        PropFormula { 
            root: Symbol::Unary(UnaryOp::Not), 
            left: None, 
            right: Some(Box::new(self)),
            size: sz + 1
        }
    }

    /// Merge two formulas with a connective,
    /// creating a new formula. Consumes the 
    /// two formulas; if combining two subformulas
    /// of the same formula just clone() as needed.
    pub fn combine(self, connective: BinaryOp, second: Self) -> Self {
        let (sz1, sz2) = (self.size, second.size);
        PropFormula {
            root: Symbol::Binary(connective),
            left: Some(Box::new(self)),
            right: Some(Box::new(second)),
            size: sz1 + sz2 + 1
        }
    }

    /// Same as combine but self placed on the right.
    pub fn left_combine(self, connective: BinaryOp, second: Self) -> Self {
        let (sz1, sz2) = (self.size, second.size);
        PropFormula {
            root: Symbol::Binary(connective),
            left: Some(Box::new(second)),
            right: Some(Box::new(self)),
            size: sz1 + sz2 + 1
        }
    }

    /// Swap the left and right children of a formula. Formula must have two children.
    pub fn swap_children(self) -> Self {
        if let Symbol::Binary(b) = self.root {
            let (left, right) = self.take_children();
            left.combine(b, right)
        } else {self}
    }

    /// Change the connective of a formula.
    pub fn recombine(self, connective: BinaryOp) -> Self {
        let mut formula = self;
        if let Symbol::Binary(_) = &formula.root {
            formula.root = Symbol::Binary(connective);
        }
        formula
    }

    /// Given a method on PropFormula that produces another PropFormula,
    /// apply it to all the subtrees of the formula!
    fn subtree_recurse<F>(self, method: &F) -> Self 
    where F: Fn(Self) -> Self 
    {
        let mut formula = self;
        match formula.root {
            Symbol::Binary(_) => {
                formula = formula.possess_children(|f| f.subtree_recurse(method), |f| f.subtree_recurse(method));
            },
            Symbol::Unary(_) => {
                formula = formula.possess_child(true, |f| f.subtree_recurse(method));
            },
            _ => {}
        }
        method(formula)
    }


    /// Convert an implies formula to negation disjunction form.
    fn de_imply(self) -> Self {
        if let Symbol::Binary(BinaryOp::Implies) = self.root {
            self.possess_child(false, &|f: PropFormula| f.negate())
                .recombine(BinaryOp::Or)
        }
        else {self}
    }
    
    /// Convert a biconditional formula to negation conjunction form.
    fn de_iff(self) -> Self {
        if let Symbol::Binary(BinaryOp::Iff) = self.root {
            let backward = self.clone()
                                            .swap_children()
                                            .recombine(BinaryOp::Implies)
                                            .de_imply();
            self.recombine(BinaryOp::Implies)
                .de_imply()
                .combine(BinaryOp::And, backward)
        } else {self}
    }

    /// Remove implies and iff operators by using the normal definitions.
    /// Assumes formula is valid. 
    fn eliminate_implies(self) -> Self {
        self.subtree_recurse(&|formula| formula.de_imply().de_iff())
    }

    /// De Morgan's Law 1: convert 'not (a and b)' to 'not a' or 'not b'
    fn and_to_or(self) -> Self {
        if let (Symbol::Unary(UnaryOp::Not), Symbol::Binary(BinaryOp::And)) = (self.root, self.get_child(true).root) {
            self.possess_children(|f| f.negate(), |f| f.negate())
                .recombine(BinaryOp::Or)
        } else {self}
    }

    /// De Morgan's Law 2: 'not a or b' to 'not a' and 'not b'
    fn or_to_and(self) -> Self {
        if let (Symbol::Unary(UnaryOp::Not), Symbol::Binary(BinaryOp::Or)) = (&self.root, &self.get_child(true).root) {
            self.possess_children(|f| f.negate(), |f| f.negate())
                .recombine(BinaryOp::And)
        } else {self}
    }

    /// Apply de Morgan's laws to conjunctions/disjunctions.
    /// Assumes formula is valid, so unwrap() called without checking.
    fn de_morgans(self) -> Self {
        self.subtree_recurse(&|formula| formula.and_to_or().or_to_and())
    }

    /// Remove redundant negations on current subformula in-place. Another method assuming validity.
    fn de_negate(self) -> Self {
        let mut formula = self;
        while let Some(_) = formula.right {
            if let (Symbol::Unary(UnaryOp::Not), Symbol::Unary(UnaryOp::Not)) = (formula.root, formula.get_child(true).root) {
                formula = formula.take_child(true).take_child(true);
            } else {break;}
        }
        formula
    }

    /// Remove multiple negations across a whole formula.
    fn strip_negate(self) -> Self {
        self.subtree_recurse(&|formula| formula.de_negate())
    }

    /// Distribute conjunction across disjunction! 
    /// That is, a ∨ (b ∧ c) becomes (a ∨ b) ∧ (a ∨ c) and 
    /// (a ∧ b) ∨ c becomes (a ∨ c) ∧ (b ∨ c)
    fn split_conjunction(self) -> Self {
        let mut formula = self;
        if let Some(_) = formula.right {
            if let (Symbol::Binary(BinaryOp::Or), Symbol::Binary(BinaryOp::And)) = 
                   (formula.root, formula.get_child(true).root) 
            {
                let a = formula.get_child(false).clone();
                formula = formula.rotate_left().possess_child(true, 
                    move|f|  f.left_combine(BinaryOp::Or, a));
            }
        } if let Some(_) = formula.left {
            if let (Symbol::Binary(BinaryOp::Or), Symbol::Binary(BinaryOp::And)) = 
                   (formula.root, formula.get_child(false).root) 
            {
                let c = formula.get_child(true).clone();
                formula = formula.rotate_right().possess_child(false, 
                    move|f| f.combine(BinaryOp::Or, c));
            }
        }
        formula
    }

    /// Distribute conjunctions throughout all subformulas of a formula!
    fn distribute_conjunction(self) -> Self {
        self.subtree_recurse(&|f| f.split_conjunction())
    }

    
    /// Given an index into current theorems, perform modus ponens with that formula
    /// on current formula if possible.
    fn exec_modus_ponens(&self, other: &PropFormula) -> Result<PropFormula, MPErr> {
        if let None = self.left {Err(MPErr::InvalidIndex)}
        else if let Symbol::Binary(BinaryOp::Implies) = self.root {
            if self.get_child(false) != other {Err(MPErr::NotAntecedent)}
            else {Ok(self.get_child(true).clone())}
        } else {Err(MPErr::NotImplies)}
    }

    /// See if modus ponens is possible on current formula with current theorems by linear search.
    fn try_modus_ponens(&self, theorems: &Proof) -> Result<PropFormula, MPErr> {
        let mut antecedent: Option<&PropFormula> = None;
        if let Some(_) = self.left {antecedent = Some(self.get_child(false))}
        for theorem in theorems {
            if let Some(left_side) = antecedent {
                if left_side == theorem {return self.exec_modus_ponens(theorem)}
            } if let Some(_) = theorem.left {
                if self == theorem.get_child(false) {return theorem.exec_modus_ponens(self)}
            }
        } Err(MPErr::NoFormulas)
    }


    /// Convert a string to a PropFormula!
    fn from_string(string: &str) -> Result<PropFormula, ValidationError> {
        let parsed: ParsedSymbols = string.into();
        if let Err(err) = parsed.0 {Err(ValidationError::ParseError(err))} 
        else {PropFormula::construct_from_symbols(&parsed.0.unwrap()[..])}
    }
    
    /// Construct from a slice of symbols.
    /// Assumes parentheses are balanced in symbols slice.
    /// Returns ValidationError if the formula being constructed isn't valid!
    fn construct_from_symbols(symbols: &[Symbol]) -> Result<PropFormula, ValidationError> {
        if symbols.is_empty() {return Err(ValidationError::EmptyFormula);}
        if symbols.len() == 1 {    // 'base case': just an atom!
        if let Symbol::Atom(a) = symbols[0] {return Ok(PropFormula::new(a))}
        else {return Err(ValidationError::NoLiteral)}
    } 
    // handle parentheses if they wrap the whole formula
    if symbols[0] == Symbol::Left 
    && PropFormula::matching_parentheses(symbols, 0)? == symbols.len() - 1 {
        return PropFormula::construct_from_symbols(&symbols[1..symbols.len()-1]);
    }
    // find the lowest precedence operator that's NOT in parentheses!
    let mut depth = 0;
    let (mut idx, mut symbol): (Option<usize>, Option<Symbol>) = (None, None);
    for (i, s) in symbols.iter().enumerate().rev() {
        match s {
            Symbol::Binary(_) | Symbol::Unary(_) => {
                    if let Some(sym) = symbol {
                        if depth == 0 && *s <= sym {(idx, symbol) = (Some(i), Some(*s))}
                    } else if depth == 0 {(idx, symbol) = (Some(i), Some(*s))}
                },
                Symbol::Atom(_) => {},
                Symbol::Left => {
                    depth -= 1;
                    if depth < 0 {
                        symbols[i..].iter().for_each(|s| print!("{}", s.to_string()));
                        print!("\n");
                        return Err(ValidationError::ParseError(SymbolParseError::UnbalancedParentheses));
                    }
                },
                Symbol::Right => {depth += 1},
            }
        }
        match symbol {
            Some(Symbol::Binary(_)) => {PropFormula::construct_from_binary(symbols, idx.unwrap())},
            Some(Symbol::Unary(u)) => {
                if idx.unwrap() != 0 {Err(ValidationError::Unary(u))}
                else {Ok(PropFormula::construct_from_symbols(&symbols[1..])?.negate())}
            },
            _ => {Err(ValidationError::NoConnectives)}
        }
    }
    
    /// A subroutine for formula construction: given an index of a binary operator in a slice 
    /// of symbols, recursively construct around it and combine or return error if not possible!
    fn construct_from_binary(symbols: &[Symbol], idx: usize) -> Result<PropFormula, ValidationError> {
        if let Symbol::Binary(b) = symbols[idx] {
            if idx == 0 || idx == symbols.len() - 1 {Err(ValidationError::Binary(b))}
            else {
                Ok(PropFormula::construct_from_symbols(&symbols[..idx])?
                .combine(b, PropFormula::construct_from_symbols(&symbols[idx+1..])?))
            }
        } else {Err(ValidationError::MismatchedSymbol(symbols[idx]))}
    }
    

    /// A subroutine for finding the corresponding right parenthesis in a symbol slice with
    /// balanced parentheses in it. Assumes idx passed in actually has a left parenthesis in it.
    fn matching_parentheses(symbols: &[Symbol], idx: usize) -> Result<usize, ValidationError> {
        let mut depth: usize = 1;
        let mut right: usize = symbols.len();
        for i in idx + 1..symbols.len() {
            if let Symbol::Right = symbols[i] {
                depth -= 1; 
                if depth == 0 {right = i; break;}
            } else if let Symbol::Left = symbols[i] {depth += 1;}
        } 
        if depth != 0 {Err(ValidationError::ParseError(SymbolParseError::UnbalancedParentheses))}
        else {Ok(right)}
    }
    

    fn subformula_traverse(&self, subformulas: &mut HashSet<PropFormula>) -> Option<&Self> {
        match subformulas.get(self) {
            Some(_) => Some(self),
            None => {
                subformulas.insert(self.clone());
                let (left, right) = 
                (self.left.as_ref().map_or(None, |f| f.subformula_traverse(subformulas)),
                 self.right.as_ref().map_or(None, |f| f.subformula_traverse(subformulas)));
                match (left, right) {
                    (None, None) => {None},
                    (None, Some(f)) | (Some(f), None) => Some(f),
                    (Some(f1), Some(f2)) => {
                        if f1.size > f2.size {Some(f1)}
                        else {Some(f2)}
                    },
                }
            },
        }
    }

    fn get_edges(&self, idx: &mut usize, edges: &mut Vec<Vec<usize>>) {
        let self_idx = *idx;
        if let Some(left_child) = &self.left {
            *idx += 1;
            edges.push(vec![self_idx, *idx]);
            left_child.get_edges(idx, edges);
        }
        if let Some(right_child) = &self.right {
            *idx += 1;
            edges.push(vec![self_idx, *idx]);
            right_child.get_edges(idx, edges);
        }
    } 
}

impl Display for PropFormula {
    /// Prints inorder traversal of propositional formula.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_inorder_string())
    }
}

/// A Python-facing interface for PropFormula.
#[pymethods]
impl PropFormula {
    /// Python initializer. Either initialize a random from a passed in set or an atom from
    /// passed in str atom: ONLY THE FIRST CHARACTER of atom will be used!
    #[new]
    #[pyo3(signature = (atom=None, random=false, atoms=None))]
    pub fn new_atom(atom: Option<&str>, random: bool, atoms: Option<HashSet<char>>) -> PyResult<Self> {
        if random {
            if let Some(set) = atoms {
                if let Some(c) = set.iter().choose(&mut thread_rng()) {
                    Ok(PropFormula::new(*c))
                } else {Err(PyValueError::new_err("set of atoms must be non-empty!"))}
            } else {
                Err(PyValueError::new_err("if you want a random atom, set of atoms must be provided"))
            }
        }
        else {
            if let Some(s) = atom {Ok(PropFormula::new(s.chars().next().unwrap()))}
            else {Err(PyValueError::new_err("atom must be provided"))}
        }
    }

    pub fn edges(&self) -> PyResult<Vec<Vec<usize>>> {
        let mut edges = Vec::new();
        let mut counter: usize = 0;
        self.get_edges(&mut counter, &mut edges);
        Ok(edges)
    }

    pub fn nodes(&self) -> PyResult<Vec<usize>> {
        Ok(self.to_preorder_sequence()
           .iter()
           .map(|s| s.to_integer())
           .collect::<Vec<_>>())
    }

    /// Connect two formulae with implication, returning a copy.
    pub fn connect(&self, other: &PropFormula) -> PyResult<Self> {
        Ok(self.clone().combine(BinaryOp::Implies, other.clone()))
    }

    /// Negate a formula and return a copy.
    pub fn refute(&self) -> PyResult<Self> {
        Ok(self.clone().negate())
    }

    pub fn conjunctive_normal_form(&self) -> PyResult<Self> {
        self.validate()?;
        Ok(self.clone()
               .eliminate_implies()
               .de_morgans()
               .strip_negate()
               .distribute_conjunction())
    }

    pub fn get_atoms(&self) -> HashSet<char> {
        let mut atoms: HashSet<char> = HashSet::new();
        self.traverse_atoms(&mut atoms);
        atoms
    }

    /// Instantiate certain atoms in a formula with other formulae. 
    /// Returns a copy, assumes formula is valid!
    pub fn instantiate(&self, atomic_formulas: HashMap<char, PropFormula>) -> Self {
        self.clone().subtree_recurse(&|f: PropFormula| f.instantiate_atom(&atomic_formulas))
    }


    /// Check recursively if a formula is an instance of another axiom, a
    /// wrapper around match_traverse so user doesn't have to always instantiate
    /// a new HashMap.
    pub fn is_instance(&self, axiom: &PropFormula) -> bool {
        self.match_traverse(axiom, &mut HashMap::new())
    }

    pub fn equals(&self, other: &PropFormula) -> bool {
        self == other
    }

    pub fn __str__(&self) -> String {
        self.to_string()
    }

    pub fn __repr__(&self) -> String {
        self.to_string()
    }

    pub fn __len__(&self) -> usize {
        self.size
    }

    pub fn modus_ponens(&self, theorems: Proof) -> PyResult<Self> {
        Ok(self.try_modus_ponens(&theorems)?)
    } 


    /// Preorder or inorder string, all in one.
    #[pyo3(signature = (preorder=false))]
    pub fn convert_to_string(&self, preorder: bool) -> PyResult<String> {
        if preorder {Ok(self.to_preorder_string())}
        else {Ok(self.to_inorder_string())}
    }

    /// Generate a random PropFormula, which may contain any capital-letter atoms and
    /// be negated or binary connected in any manner of ways.
    /// The maximum number of subformulas can be specified by the max_iter parameter.
    #[pyo3(signature = (max_iter=5, atoms=None))]
    #[staticmethod]
    pub fn random_generate(max_iter: Option<usize>, atoms: Option<HashSet<char>>) -> Self { 
        let num_iter;
        let mut rng = StdRng::from_rng(thread_rng()).unwrap();
        if let Some(num) = max_iter {num_iter = rng.gen_range(1..=num);}
        else {num_iter = random();}
        
        // start with an atom on the 0th iteration
        let mut formula = PropFormula::random_atom(atoms.as_ref());
        // at any time you can combine or you can negate
        for _ in 0..num_iter {
            if thread_rng().gen_bool(0.2) {
                formula = formula.negate();
            } else {
                let bin = match rng.gen_range(1..=4) {
                    1 => {BinaryOp::And},
                    2 => {BinaryOp::Or},
                    3 => {BinaryOp::Implies},
                    _ => {BinaryOp::Iff},
                };
                formula = formula.combine(bin, PropFormula::random_atom(atoms.as_ref()));
            }
        }
        formula

    }

    /// Generate a random PropFormula, which may contain any capital-letter atoms and
    /// be negated or binary connected with implication.
    /// The maximum number of subformulas can be specified by the max_iter parameter.
    #[pyo3(signature = (max_iter=5, atoms=None))]
    #[staticmethod]
    pub fn hilbert_generate(max_iter: Option<usize>, atoms: Option<HashSet<char>>) -> Self { 
        let num_iter;
        let mut rng = StdRng::from_rng(thread_rng()).unwrap();
        if let Some(num) = max_iter {num_iter = rng.gen_range(1..=num);}
        else {num_iter = random();}
        
        // start with an atom on the 0th iteration
        let mut formula = PropFormula::random_atom(atoms.as_ref());
        // at any time you can combine or you can negate
        for _ in 0..num_iter {
            if random() {
                formula = formula.negate();
            } else {
                formula = formula.combine(BinaryOp::Implies, PropFormula::random_atom(atoms.as_ref()));
            }
        }
        formula
    }

    /// Wrapper for from_string() for Python.
    #[staticmethod]
    pub fn string_to_formula(string: &str) -> PyResult<Self> {
        match PropFormula::from_string(string) {
            Ok(formula) => Ok(formula),
            Err(err) => Err(err.into())
        }
    }

    #[staticmethod]
    pub fn largest_common_subformula(first: &PropFormula, second: &PropFormula) -> PyResult<usize> {
        let mut hashed: HashSet<PropFormula> = HashSet::new();
        first.subformula_traverse(&mut hashed);
        Ok(second.subformula_traverse(&mut hashed).map_or(0, |f| f.size))
    }

    /// Return the symmetry axiom, a → b → a.
    #[staticmethod]
    pub fn symmetry() -> Self {
        PropFormula::new('A')
        .combine(BinaryOp::Implies, PropFormula::new('B')
        .combine(BinaryOp::Implies, PropFormula::new('A')))
    }
    /// Return the distribution axiom, (a → (b → c)) → (a → b) → b → c
    #[staticmethod]
    pub fn distribution() -> Self {
        let ab = PropFormula::new('A')
                              .combine(BinaryOp::Implies, 
                              PropFormula::new('B'));
        let bc1 = PropFormula::new('B')
                               .combine(BinaryOp::Implies, 
                               PropFormula::new('C'));
        let bc2 = bc1.clone();
        PropFormula::new('A').combine(BinaryOp::Implies, bc1)
                             .combine(BinaryOp::Implies, ab.combine(BinaryOp::Implies, bc2))
    }
    /// Return the contraposition axiom, (a → b) → ¬b → ¬a 
    #[staticmethod]
    pub fn contraposition() -> Self {
        PropFormula::new('A')
        .combine(BinaryOp::Implies, PropFormula::new('B'))
        .combine(BinaryOp::Implies, PropFormula::new('B')
        .combine(BinaryOp::Implies, PropFormula::new('A'))
        .possess_children(|f| f.negate(), |f| f.negate()))
    }
    
    #[staticmethod]
    #[pyo3(signature = (num_iter=10, atomset=None, print=true))]
    pub fn generate_theorem(num_iter: usize, atomset: Option<HashSet<char>>, print: bool) -> PyResult<Self> {
        if num_iter < 1 {return Err(PyValueError::new_err("must have greater than 0 number of iterations"))}
        let mut formulas: Vec<PropFormula> = vec![];
        let mut formula: PropFormula = PropFormula::hilbert_generate(Some(5), atomset.clone());
        formulas.push(formula.clone());
        for _ in 1..num_iter {
            let mut num_options: usize = 3;
            let mut mp: Option<PropFormula> = None;
            if let Ok(mp_formula) = formula.try_modus_ponens(&formulas) {
                num_options += 1;
                mp = Some(mp_formula);
            }

            let mut atoms: HashMap<char, PropFormula> = HashMap::new();
            atoms.insert('A', PropFormula::hilbert_generate(Some(5), atomset.clone()));
            atoms.insert('B', PropFormula::hilbert_generate(Some(5), atomset.clone()));
            
            match thread_rng().gen_range(1..=num_options) {
                1 => {formula = PropFormula::symmetry().instantiate(atoms)}
                2 => {formula = PropFormula::contraposition().instantiate(atoms)}
                3 => {
                    atoms.insert('C', PropFormula::hilbert_generate(Some(5), atomset.clone()));
                    formula = PropFormula::distribution().instantiate(atoms)
                }
                _ => {formula = mp.unwrap()}
            }
            formulas.push(formula.clone());
        }
        if print {
            formulas.iter().for_each(|s| println!("{}", s.to_string()));
        }
        Ok(formula) 
    }
}

impl PartialEq for PropFormula {
    /// This implementation of equality is NOT logical equality, which should use
    /// CNF. This is literally about equality of symbols.
    fn eq(&self, other: &Self) -> bool {
        self.to_preorder_sequence() == other.to_preorder_sequence()
    }
}

impl Eq for PropFormula {}

struct ParsedSymbols(Result<Vec<Symbol>, SymbolParseError>);

impl From<&str> for ParsedSymbols {
    fn from(value: &str) -> Self {
        let mut symbols: Vec<Symbol> = vec![];
        let mut left: usize = 0;
        let mut right: usize = 0;
        for ch in value.chars() {
            if ch.is_alphabetic() {symbols.push(Symbol::Atom(ch)); continue;}
            match ch {
                ' ' => {continue;},
                '↔' => {symbols.push(Symbol::Binary(BinaryOp::Iff))},
                '→' => {symbols.push(Symbol::Binary(BinaryOp::Implies))},
                '∧' => {symbols.push(Symbol::Binary(BinaryOp::And))},
                '∨' => {symbols.push(Symbol::Binary(BinaryOp::Or))},
                '¬' => {symbols.push(Symbol::Unary(UnaryOp::Not))},
                '(' => {
                    left += 1; 
                    symbols.push(Symbol::Left)
                },
                ')' => {
                    right += 1; 
                    if right > left {return ParsedSymbols(Err(SymbolParseError::UnbalancedParentheses));} 
                    symbols.push(Symbol::Right);
                },
                _ => {return ParsedSymbols(Err(SymbolParseError::InvalidChar(ch)))}                     
            }
        }
        ParsedSymbols(Ok(symbols))
    }
}

type Proof = Vec<PropFormula>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MPErr {
    InvalidIndex,
    NotAntecedent,
    NotImplies,
    NoFormulas
}

impl From<MPErr> for PyErr {
    fn from(value: MPErr) -> Self {
        PyValueError::new_err(value.to_string())
    }
}

impl Display for MPErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MPErr::InvalidIndex => {write!(f, "Not a valid index into the proof.")}
            MPErr::NotAntecedent => {write!(f, "This is not an antecedent of the current formula.")},
            MPErr::NotImplies => {write!(f, "This formula is not an implication.")},
            MPErr::NoFormulas => {write!(f, "No formulas in the proof you gave can do modus ponens, sorry.")},
        }
    }
}






#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{MPErr, Proof};

    use super::{PropFormula, Symbol, BinaryOp, UnaryOp, ValidationError};

    #[test]
    fn inorder_sequence() -> Result<(), ValidationError> {
        let mut formula = PropFormula::new('p');
        assert_eq!("p", formula.to_string());
        formula = formula.negate();
        assert_eq!("¬p", formula.to_string());
        formula = formula.combine(BinaryOp::Iff, PropFormula::new('q'));
        formula.validate()?;
        assert_eq!(vec![Symbol::Unary(UnaryOp::Not), Symbol::Atom('p'), Symbol::Binary(BinaryOp::Iff), Symbol::Atom('q')], 
                   formula.to_inorder_sequence());
        assert_eq!("¬p ↔ q", formula.to_string());
        // let's do a more complicated precedence example
        let test = PropFormula::from_string("(p → q) → (r ∧ s)")?;
        assert_eq!("(p → q) → r ∧ s", test.to_string());    // we're assuming that to_string() does least parentheses possible
        Ok(())
    }

    
    #[test]
    fn preorder_sequence() -> Result<(), ValidationError> {
        let mut formula = PropFormula::new('p');
        assert_eq!("p", formula.to_preorder_string()); 
        formula = formula.negate();
        assert_eq!(vec![Symbol::Unary(UnaryOp::Not), Symbol::Atom('p')], formula.to_preorder_sequence());
        assert_eq!("¬p", formula.to_preorder_string()); 
        formula = formula.combine(BinaryOp::Iff, PropFormula::new('q'));
        formula.validate()?;
        assert_eq!("¬p ↔ q", formula.to_string());
        assert_eq!(vec![Symbol::Binary(BinaryOp::Iff), Symbol::Unary(UnaryOp::Not), Symbol::Atom('p'), Symbol::Atom('q')],
                   formula.to_preorder_sequence());
        assert_eq!(" ↔ ¬pq", formula.to_preorder_string()); 
        Ok(())
    }

    /// Just test lots of different kinds of formulas to string.
    /// May not be totally comprehensive but we test against randomized formulas.
    #[test]
    fn string_to_formula_testing() -> Result<(), ValidationError> {
        let formula = PropFormula::new('p')
                                                    .negate()
                                                    .combine(BinaryOp::Iff, PropFormula::new('q'));
        formula.validate()?;
        let from_str = PropFormula::from_string("¬p ↔ q")?;
        assert_eq!(from_str, formula);
        assert_ne!(from_str, PropFormula::new('p').combine(BinaryOp::Iff, PropFormula::new('q')).negate());
        // let's try associativity!
        let mut precedence = PropFormula::from_string("p → q → r")?;
        let right = PropFormula::new('p')
                                                .combine(BinaryOp::Implies, 
                            PropFormula::new('q')
                                                .combine(BinaryOp::Implies, PropFormula::new('r')));
        let wrong = PropFormula::new('p')
                                                .combine(BinaryOp::Implies, 
                            PropFormula::new('q'))
                                                .combine(BinaryOp::Implies, PropFormula::new('r'));
        assert_eq!(right, precedence);
        assert_ne!(wrong, precedence);
        // precedence + multiple operators!
        precedence = precedence.combine(BinaryOp::And, PropFormula::new('s'));
        assert_eq!(precedence, PropFormula::from_string("(p → q → r) ∧ s")?);
        assert_eq!(precedence, PropFormula::from_string("(p → (q → r)) ∧ s")?);
        assert_ne!(precedence, PropFormula::from_string("((p → q) → r) ∧ s")?);
        assert_ne!(precedence, PropFormula::from_string("p → q → r ∧ s")?);
        // let's try an axiom and different parenthesization levels
        let complex = PropFormula::distribution();
        assert_eq!(PropFormula::from_string("(A → (B → C)) → (A → B) → B → C")?, complex);
        assert_eq!(PropFormula::from_string("(A → (B → C)) → ((A → B) → B → C)")?, complex);
        assert_eq!(PropFormula::from_string("(A → (B → C)) → ((A → B) → (B → C))")?, complex);
        assert_eq!(PropFormula::from_string("((A → (B → C)) → ((A → B) → (B → C)))")?, complex);
        // finally repeated negation!
        assert_eq!(PropFormula::from_string("¬¬¬d")?, PropFormula::new('d').negate().negate().negate());
        for _ in 1..=75 {    // "robust" testing against whatever wacky formulas generate() makes
            let random = PropFormula::random_generate(Some(75), None);
            assert_eq!(PropFormula::from_string(&random.to_string())?, random);
        }
        Ok(())
    }


    #[test]
    fn instantiation() -> Result<(), ValidationError> {
        let mut axiom = PropFormula::distribution();
        assert_eq!(PropFormula::from_string("(A → (B → C)) → (A → B) → B → C")?, axiom);
        let mut atomic_formulas = HashMap::new();
        atomic_formulas.insert('A', PropFormula::from_string("p → q → r")?);
        axiom = axiom.instantiate(atomic_formulas.clone());
        assert_eq!(PropFormula::from_string("((p → q → r) → (B → C)) → ((p → q → r) → B) → B → C")?, axiom);
        atomic_formulas.insert('B', PropFormula::from_string("f ∧ g → h")?);
        axiom = axiom.instantiate(atomic_formulas.clone());
        assert_eq!(PropFormula::from_string("((p → q → r) → ((f ∧ g → h) → C)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → C")?, axiom);
        atomic_formulas.insert('C', PropFormula::from_string("¬¬¬d")?);
        axiom = axiom.instantiate(atomic_formulas.clone());
        assert_eq!(PropFormula::from_string("((p → q → r) → ((f ∧ g → h) → ¬¬¬d)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → ¬¬¬d")?, axiom);
        Ok(())
    }
    
    
    #[test]
    fn check_instancing() -> Result<(), ValidationError> {
        assert!(PropFormula::from_string("((p → q → r) → ((f ∧ g → h) → ¬¬¬d)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → ¬¬¬d")?
                            .is_instance(&PropFormula::distribution()));
        assert!(!PropFormula::from_string("(p → q → r)")?.is_instance(&PropFormula::symmetry()));
        assert!(PropFormula::from_string("(p → q → r) → (p → q → r) → (p → q → r)")?.is_instance(&PropFormula::symmetry()));
        assert!(PropFormula::from_string("((p → q) → (¬q → ¬p))")?.is_instance(&PropFormula::contraposition()));
        assert!(!PropFormula::from_string("(p → q → ¬q → ¬p)")?.is_instance(&PropFormula::contraposition()));
        Ok(())
    }
    
    #[test]
    fn validate_generate() -> Result<(), ValidationError> {
        for i in 0..75 {
            let formula = PropFormula::random_generate(Some(50), None);
            formula.validate()?;
            if i % 25 == 0 {println!("{}", formula.to_string())}
        }
        Ok(())
    }
    
    // TODO these if conjunctive normal form is going to be used in the end!
    // #[test]
    // fn implies_to_conjunct() -> Result<(), ValidationError> {
    //     let test = PropFormula::from_string("¬(p → q) ∧ r ↔ s")?.eliminate_implies();
    // }

    // #[test]
    // fn double_negation() -> Result<(), ValidationError> {todo!()}

    // #[test]
    // fn rotations() -> Result<(), ValidationError> {todo!()}

    // #[test]
    // fn distribution() -> Result<(), ValidationError> {todo!()}


    #[test]
    fn similarity() -> Result<(), ValidationError> {
        let first = PropFormula::from_string("(p → q) → r")?;
        let second = PropFormula::from_string("p → r")?;
        let third = PropFormula::from_string("p → q")?;
        assert_eq!(PropFormula::largest_common_subformula(&first, &second).unwrap(), 1);
        assert_eq!(PropFormula::largest_common_subformula(&first, &third).unwrap(), 3);
        assert_eq!(PropFormula::largest_common_subformula(&second, &third).unwrap(), 1);
        Ok(())
    }

    #[test]
    fn mp_testing() -> Result<(), MPErr> {
        let proof: Proof = vec![PropFormula::from_string("(p → q) → r").unwrap(), 
                                PropFormula::from_string("p → q → r").unwrap()];
        PropFormula::from_string("p").unwrap().try_modus_ponens(&proof)?;
        PropFormula::from_string("p → q").unwrap().try_modus_ponens(&proof)?;
        assert!(PropFormula::from_string("q → r").unwrap().try_modus_ponens(&proof).is_err());
        Ok(())
    }
}


/// A Python logic module implemented in Rust.
#[pymodule]
fn prop_logic(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PropFormula>()?;
    Ok(())
}

