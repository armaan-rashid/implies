use super::symbol::{Parsable, ParseError, Symbol, Symbolic};
use crate::symbol::ParsedSymbols;
use cascade::cascade;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    Binary {
        conn: B,
        left: Box<Tree<B, U, A>>,
        right: Box<Tree<B, U, A>>,
    },
    Unary {
        conn: U,
        next: Box<Tree<B, U, A>>,
    },
    Atom(A),
}

impl<B, U, A> Display for Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut repr = String::new();
        self.read_inorder(&mut repr);
        write!(f, "{}", repr)
    }
}

impl<B, U, A> std::default::Default for Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    /// Assuming `A::default()` returns a 0 value, effectively amounts
    /// to a null value without allowing invalid trees to be constructed.
    fn default() -> Self {
        Tree::Atom(A::default())
    }
}

/// A basic binary/unary tree type for representing generic logical formulae.
/// Within the formula struct, represents the untraversed/'unzipped' parts of the
/// formula. Only the most basic manipulations (adding unary operators and combining
/// formulas) are provided; more complex manipulations are provided by the Zipper
/// which is much more ergonomic for expressing in-place mutation.
impl<B, U, A> Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    pub fn is_atomic(&self) -> bool {
        if let Tree::Atom(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unary(&self) -> bool {
        if let Tree::Unary { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_binary(&self) -> bool {
        if let Tree::Binary { .. } = self {
            true
        } else {
            false
        }
    }

    /// Combine two trees with a binary operator, inserting the new tree
    /// on the right side.
    pub fn combine(&mut self, bin: B, formula: Self) {
        let old = std::mem::take(self);
        *self = Tree::Binary {
            conn: bin,
            left: Box::new(old),
            right: Box::new(formula),
        }
    }

    /// Combine with new tree on the left!
    pub fn left_combine(&mut self, bin: B, formula: Self) {
        let old = std::mem::take(self);
        *self = Tree::Binary {
            conn: bin,
            left: Box::new(formula),
            right: Box::new(old),
        }
    }

    /// Add a unary operator to the existing formula.
    pub fn unify(&mut self, un: U) {
        let old = std::mem::take(self);
        *self = Tree::Unary {
            conn: un,
            next: Box::new(old),
        }
    }

    /// Print the customary inorder traversal of a tree formula into an outparameter.
    pub fn read_inorder(&self, repr: &mut String) {
        match self {
            Tree::Binary { conn, left, right } => {
                if Symbol::from_tree(left.as_ref()) <= Symbol::Binary(*conn) {
                    repr.push_str("(");
                    left.read_inorder(repr);
                    repr.push_str(")");
                } else {
                    left.read_inorder(repr)
                };
                repr.push_str(&conn.to_string());
                if Symbol::from_tree(right.as_ref()) < Symbol::Binary(*conn) {
                    repr.push_str("(");
                    right.read_inorder(repr);
                    repr.push_str(")");
                } else {
                    right.read_inorder(repr)
                }
            }
            Tree::Unary { conn, next } => {
                repr.push_str(&conn.to_string());
                if Symbol::from_tree(next.as_ref()) < Symbol::Unary(*conn) {
                    repr.push_str("(");
                    next.read_inorder(repr);
                    repr.push_str(")");
                } else {
                    next.read_inorder(repr)
                }
            }
            Tree::Atom(a) => repr.push_str(&a.to_string()),
        }
    }

    /// Recursively build a formula from a slice of symbols.
    pub fn build_tree(syms: &[Symbol<B, U, A>]) -> Result<Self, ParseError> {
        match (syms.first(), syms.last()) {
            (Some(s1), Some(s2)) => {
                if let (Symbol::Left, Symbol::Right) = (s1, s2) {
                    Self::build_tree(&syms[1..syms.len() - 1])
                } else {
                    match Symbol::lowest_precedence(syms)? {
                        (i, Symbol::Binary(b)) => Ok(Tree::Binary {
                            conn: b,
                            left: Box::new(Self::build_tree(&syms[..i])?),
                            right: Box::new(Self::build_tree(&syms[i + 1..])?),
                        }),
                        (i, Symbol::Unary(u)) => {
                            if i != 1 {
                                Err(ParseError::DisconnectedClauses)
                            } else {
                                Ok(Tree::Unary {
                                    conn: u,
                                    next: Box::new(Self::build_tree(&syms[i..])?),
                                })
                            }
                        }
                        (_, Symbol::Atom(a)) => Ok(Tree::Atom(a)),
                        _ => Err(ParseError::DisconnectedClauses),
                    }
                }
            }
            _ => Err(ParseError::EmptyFormula),
        }
    }
}

/// The thread or 'zipper' that actually tracks where you currently
/// are in a given tree formula. The recursively nested zippers themselves
/// contain the node values that trace out a partial walk from the head
/// of the tree. Zippers contain trees themselves if and only if they make
/// a 'choice' during the walk, e.g. they traverse one of two binary subtrees,
/// to retain the choice not made.
#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug, Default)]
pub enum Zipper<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    #[default]
    Top,
    Right {
        bin: B,
        sub: Tree<B, U, A>,
        zip: Box<Zipper<B, U, A>>,
    },
    Left {
        bin: B,
        sub: Tree<B, U, A>,
        zip: Box<Zipper<B, U, A>>,
    },
    Up {
        un: U,
        zip: Box<Zipper<B, U, A>>,
    },
}

impl<B: Symbolic, U: Symbolic, A: Symbolic> Zipper<B, U, A> {
    /// For formula traversal through the zipper when
    /// the actual zipper state doesn't need to be changed.
    pub fn peek_up(&self) -> &Self {
        match self {
            Zipper::Top => self,
            Zipper::Right { zip, .. } | Zipper::Left { zip, .. } | Zipper::Up { zip, .. } => {
                zip.as_ref()
            }
        }
    }

    /// Flip a right zipper to left or vice versa while retaining
    /// all the same data.
    pub fn flip(&mut self) {
        if let Zipper::Right { bin, sub, zip } = self {
            *self = Zipper::Left {
                bin: *bin,
                sub: std::mem::take(sub),
                zip: std::mem::take(zip),
            }
        } else if let Zipper::Left { bin, sub, zip } = self {
            *self = Zipper::Right {
                bin: *bin,
                sub: std::mem::take(sub),
                zip: std::mem::take(zip),
            }
        }
    }
}

/// The primary generic struct for logical formulas that
/// implement both unary and binary operators.
/// The struct is generic over the type of binary
/// operators `B`, unary operators `U`, and the atoms `A`, and
/// assumes all three are very cheap (e.g. fieldless enums, integers)
/// and therefore implement Copy. It's possible this requirement will
/// be relaxed in future versions for the atoms 'A', in case there's
/// a need for very complex atoms (i.e. arbitrarily large relations).
///
/// This is a [zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
/// implementation of binary/unary trees which represent logical formulae. This means that a `Formula` is not
/// just a tree, but a tree *and a particular location in that tree*, represented by the `zipper`; please read
/// the source material for more if you want to understand how this works. At the basic level you may assume that
/// `.*_unzip`, methods go 'down' the tree while `.*_zip` methods go up. If the formula is fully zipped (i.e. by calling
/// `.full_zip()`) the tree is in fact the whole formula, with `Zipper::Top` stored as a sentinel zipper.
///
/// The implementation therefore always
/// operates/mutates the formula at its current location in the tree, so if you want to operate on the very 'top'
/// of the formula you must call `.zip()` first. The only exception to this is methods that start with `.full_`,
/// which you may assume call `.zip()` before performing mutations.
///
/// Finally, you may notice this `impl` does not implement the builder pattern of taking and returning `self`
/// both because the pattern is ugly but mainly because methods must operate on `&mut self` in order to be
/// bound for Python. For builder-style method chaining use the wonderful `cascade!{}` macro.
#[derive(PartialEq, Hash, Eq, PartialOrd, Ord, Clone, Debug, Default)]
pub struct Formula<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    pub tree: Tree<B, U, A>,
    pub zipper: Zipper<B, U, A>,
}

impl<B, U, A> Formula<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    pub fn zip(&mut self) {
        match &self.zipper {
            Zipper::Top => {}
            Zipper::Right { .. } => self.right_zip(),
            Zipper::Left { .. } => self.left_zip(),
            Zipper::Up { .. } => self.up_zip(),
        }
    }

    pub fn up_zip(&mut self) {
        if let Formula {
            ref mut tree,
            zipper: Zipper::Up { un, ref mut zip },
        } = self
        {
            tree.unify(*un);
            self.zipper = std::mem::take((*zip).as_mut())
        }
    }

    pub fn right_zip(&mut self) {
        if let Formula {
            ref mut tree,
            zipper:
                Zipper::Right {
                    bin,
                    ref mut sub,
                    ref mut zip,
                },
        } = self
        {
            tree.combine(*bin, std::mem::take(sub));
            self.zipper = std::mem::take((*zip).as_mut());
        }
    }

    pub fn left_zip(&mut self) {
        if let Formula {
            ref mut tree,
            zipper:
                Zipper::Left {
                    bin,
                    ref mut sub,
                    ref mut zip,
                },
        } = self
        {
            tree.left_combine(*bin, std::mem::take(sub));
            self.zipper = std::mem::take((*zip).as_mut());
        }
    }

    /// Traverse to the right subtree of a binary formula.
    pub fn right_unzip(&mut self) {
        if let Formula {
            tree:
                Tree::Binary {
                    conn,
                    ref mut left,
                    ref mut right,
                },
            ref mut zipper,
        } = self
        {
            let for_zip = std::mem::take((*left).as_mut());
            let new_tree = std::mem::take((*right).as_mut());
            let new_zip: Zipper<B, U, A> = Zipper::Left {
                bin: *conn,
                sub: for_zip,
                zip: Box::new(std::mem::take(zipper)),
            };
            *zipper = new_zip;
            self.tree = new_tree;
        }
    }

    /// Traverse to the left subtree of a binary formula.
    pub fn left_unzip(&mut self) {
        if let Formula {
            tree:
                Tree::Binary {
                    conn,
                    ref mut left,
                    ref mut right,
                },
            ref mut zipper,
        } = self
        {
            let for_zip = std::mem::take((*right).as_mut());
            let new_tree = std::mem::take((*left).as_mut());
            let new_zip: Zipper<B, U, A> = Zipper::Left {
                bin: *conn,
                sub: for_zip,
                zip: Box::new(std::mem::take(zipper)),
            };
            *zipper = new_zip;
            self.tree = new_tree;
        }
    }

    /// Traverse to the formula contained in a unary tree.
    pub fn down_unzip(&mut self) {
        if let Formula {
            tree: Tree::Unary { conn, ref mut next },
            ref mut zipper,
        } = self
        {
            let new_zip = Zipper::Up {
                un: *conn,
                zip: Box::new(std::mem::take(zipper)),
            };
            *zipper = new_zip;
            self.tree = std::mem::take((*next).as_mut());
        }
    }

    /// Unzip the formula, i.e. return to the top node.
    pub fn full_zip(&mut self) {
        loop {
            if let Zipper::Top = self.zipper {
                break;
            }
            self.up_zip()
        }
    }

    /// Combine two formulas with a binary connective.
    /// "Eats" the second formula; clone when calling this
    /// function if you want to keep the second around.
    /// This function and `.left_combine()` obviously
    /// require unzipping, so more efficient to call them
    /// if you're already unzipped.
    pub fn full_combine(&mut self, bin: B, mut formula: Self) {
        formula.full_zip();
        self.full_zip();
        self.combine(bin, formula.tree);
    }

    /// Combine two formulas with a binary connective.
    /// The second formula is inserted as the LEFT subtree.
    /// "Eats" the second formula; clone when calling this
    /// function if you want to keep the second around.
    pub fn full_left_combine(&mut self, bin: B, mut formula: Self) {
        formula.full_zip();
        self.full_zip();
        self.left_combine(bin, formula.tree)
    }

    /// Like combine but connects to a new formula WITHOUT
    /// unzipping, which is why this takes in a tree. This
    /// effectively inserts `formula` where
    /// you currently are in `self` into a right subtree.
    pub fn combine(&mut self, bin: B, new_tree: Tree<B, U, A>) {
        self.tree.combine(bin, new_tree)
    }
    /// Like insert but into the left subtree.
    pub fn left_combine(&mut self, bin: B, new_tree: Tree<B, U, A>) {
        self.tree.left_combine(bin, new_tree)
    }

    /// Insert a unary operator in the formula's current position.
    pub fn append(&mut self, un: U) {
        self.tree.unify(un)
    }

    /// Insert a unary operator for the whole formula.
    pub fn unify(&mut self, un: U) {
        cascade! {
            self;
            ..full_zip();
            ..append(un);
            ()
        }
    }

    /// Inorder traversal starting at the current context.
    /// If you want the whole formula simply unzip first.
    /// Takes in a closure which can mutate the formula in
    /// place somehow.
    pub fn inorder_traverse_mut<F: FnMut(&mut Self)>(&mut self, func: &mut F) {
        match &self.tree {
            Tree::Binary { .. } => cascade! {
                self;
                ..left_unzip();
                ..inorder_traverse_mut(func);
                ..up_zip();
                ..apply_mut(func);
                ..right_unzip();
                ..inorder_traverse_mut(func);
                ..up_zip();
                ()
            },
            Tree::Unary { .. } => cascade! {
                self;
                ..apply_mut(func);
                ..down_unzip();
                ..inorder_traverse_mut(func);
                ..up_zip();
                ()
            },
            Tree::Atom(_) => self.apply_mut(func),
        }
    }

    /// Preorder traversal starting at the current context.
    /// Also takes in a closure that can mutate the formula.
    pub fn preorder_traverse_mut<F: FnMut(&mut Self)>(&mut self, func: &mut F) {
        match &self.tree {
            Tree::Binary { .. } => cascade! {
                self;
                ..apply_mut(func);
                ..left_unzip();
                ..preorder_traverse_mut(func);
                ..up_zip();
                ..right_unzip();
                ..preorder_traverse_mut(func);
                ..up_zip();
                ()
            },
            Tree::Unary { .. } => cascade! {
                self;
                ..apply_mut(func);
                ..down_unzip();
                ..preorder_traverse_mut(func);
                ..up_zip();
                ()
            },
            Tree::Atom(_) => self.apply_mut(func),
        }
    }

    /// Purely for the sake of nicer syntax, allows closures to be called method-style
    /// as part of method chaining in the builder pattern, for builder closures this time.
    pub fn apply_mut<F: FnMut(&mut Self)>(&mut self, func: &mut F) {
        func(self);
    }

    /// If it applies in the current context, 'rotate' a tree formula,
    /// i.e. change precedence between two binary operators,
    /// to the left. As an example,
    ///
    ///     →                                       
    ///   /   \\
    /// A       ∧          
    ///       /   \
    ///     B       C
    ///
    ///         =>
    ///
    ///             ∧                               
    ///          //   \
    ///         →       C   
    ///       /   \
    ///     A       B
    ///
    /// is an example of a left rotation.
    /// Rotations are always performed assuming the current zipper holds the
    /// lower-precedence `B`, i.e. the one higher up in the tree. In the example
    /// above, the rotation would be performed on a `Formula` where the `zipper`
    /// is the \\ pictured, holding the → operator. `self` is left in the same
    /// position after rotation, with // denoting the new active zipper.
    pub fn rotate_left(&mut self) {
        if let Formula {
            tree: Tree::Binary { conn, left, right },
            zipper: Zipper::Left { bin, sub, .. },
        } = self
        {
            std::mem::swap(conn, bin);
            std::mem::swap(left.as_mut(), right.as_mut());
            std::mem::swap(sub, right.as_mut());
        }
        // You now have a right zipper, so flip it!
        self.zipper.flip()
    }
    /// If it applies in the current context, 'rotate' a tree formula,
    /// i.e. change precedence between two binary operators,
    /// to the right. As an example,
    ///
    ///          ∧                               
    ///        /   \
    ///      →       C   
    ///    /   \
    ///  A       B
    ///
    ///               =>
    ///
    ///                     →                                       
    ///                   /   \
    ///                 A       ∧          
    ///                       /   \
    ///                     B       C
    ///
    /// is an example of a right rotation.
    pub fn rotate_right(&mut self) {
        if let Formula {
            tree: Tree::Binary { conn, left, right },
            zipper: Zipper::Right { bin, sub, .. },
        } = self
        {
            std::mem::swap(conn, bin);
            std::mem::swap(left.as_mut(), right.as_mut());
            std::mem::swap(sub, left.as_mut());
        }
        // You now have a right zipper, so flip it!
        self.zipper.flip()
    }

    /// Instantiate an atom in the formula (as usual, starting where you currently are)
    /// with another tree subformula. If you want to do this over a whole formula,
    /// just call this inside `inorder_traverse_mut()`
    pub fn instantiate(&mut self, formulas: &HashMap<A, Tree<B, U, A>>) {
        if let Tree::Atom(a) = self.tree {
            if formulas.contains_key(&a) {
                self.tree = formulas[&a].clone()
            }
        }
    }

    /// Read string representation starting from current position in formula.
    pub fn read_inorder(&self) -> String {
        let mut written = String::new();
        self.tree.read_inorder(&mut written);
        let mut context: &Zipper<B, U, A> = &self.zipper;
        loop {
            match context {
                Zipper::Top => break,
                Zipper::Right { bin, sub, zip } => written += &(bin.to_string() + &sub.to_string()),
                Zipper::Left { bin, sub, zip } => {
                    written = sub.to_string() + &bin.to_string() + &written
                }
                Zipper::Up { un, zip } => written = un.to_string() + &written,
            }
            context = context.peek_up();
        }
        written
    }
}

impl<B, U, A> Display for Formula<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.read_inorder())
    }
}

impl<B: Parsable, U: Parsable, A: Parsable> Formula<B, U, A> {
    /// As expected, read a formula from a string. Return error if the string is malformed.
    pub fn from_str(value: &str) -> Result<Self, ParseError> {
        match *ParsedSymbols::from(value) {
            Ok(ref syms) => Ok(Formula {
                tree: Tree::build_tree(&syms[..])?,
                zipper: Zipper::Top,
            }),
            Err(e) => Err(e),
        }
    }
}
