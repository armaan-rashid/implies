use super::symbol::{Symbol, Symbolic};
use crate::parser::ParseError;
use cascade::cascade;
use enum_iterator::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

/// Classic tree implementation, but the enum variants
/// for Binary, Unary and Atom ensure that ill-formed
/// formulas can never be constructed. Uses [`Box`]
/// as internal pointer because we modify formulae through
/// a zipper. Within the formula struct, represents the
/// untraversed/'unzipped' parts of the formula.
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

/// Only the most basic manipulations (adding unary operators and combining
/// formulas) are provided; more complex manipulations are provided by the [`Formula`]
/// which is much more ergonomic for expressing in-place mutation using its
/// internal [`Zipper`]
/// [`Zipper`]: Zipper
/// [`Formula`]: Formula
impl<B, U, A> Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    pub fn new(atom: A) -> Self {
        Tree::Atom(atom)
    }

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

    /// Inorder traversal starting at the current zipper.
    /// Does not mutate the formula at all but the closure passed
    /// in is still [`std::ops::FnMut`] so that other state
    /// can be mutated. As usual returns [`Option<()>`] so that
    /// traversal can be stopped early by returning `None`.
    pub fn inorder_traverse<F: FnMut(&Self) -> Option<()>>(&self, func: &mut F) -> Option<()> {
        Some(match &self {
            Tree::Binary { left, right, .. } => {
                left.inorder_traverse(func);
                func(self)?;
                right.inorder_traverse(func);
            }
            Tree::Unary { next, .. } => {
                func(self)?;
                next.inorder_traverse(func);
            }
            Tree::Atom(_) => func(self)?,
        })
    }

    /// Preorder traversal starting at the current context.
    /// Also takes in a closure that can read the formula.
    pub fn preorder_traverse<F: FnMut(&Self) -> Option<()>>(&self, func: &mut F) -> Option<()> {
        func(self)?;
        Some(match &self {
            Tree::Binary { left, right, .. } => {
                left.preorder_traverse(func);
                right.preorder_traverse(func);
            }
            Tree::Unary { next, .. } => {
                next.preorder_traverse(func);
            }
            Tree::Atom(_) => {}
        })
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
}

/// The thread or 'zipper' that actually tracks where you currently
/// are in a given tree formula. The recursively nested zippers themselves
/// contain the node values that trace out a partial walk from the head
/// of the tree toward a leaf node, i.e. an atom. Zippers contain trees themselves
/// if and only if they make a 'choice' during the walk, e.g. they traverse
/// one of two binary subtrees, to retain the choice not made.
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
    pub fn is_left(&self) -> bool {
        if let Zipper::Left { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_right(&self) -> bool {
        if let Zipper::Right { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_up(&self) -> bool {
        if let Zipper::Up { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_top(&self) -> bool {
        if let Zipper::Top = self {
            true
        } else {
            false
        }
    }

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

/// If you're really lazy I guess!
impl<B, U, A> From<&Formula<B, U, A>> for Tree<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    fn from(value: &Formula<B, U, A>) -> Self {
        value.tree.clone()
    }
}

/// The primary generic struct for logical formulas that implement both unary and binary operators.
/// The struct is generic over the type of binary operators `B`, unary operators `U`, and the atoms `A`,
/// and assumes all three are very cheap (e.g. fieldless enums, integers) and therefore implement Copy.
/// It's possible this requirement will be relaxed in future versions for the atoms 'A', in case there's
/// a need for very complex atoms (i.e. arbitrarily large relations).
///
/// This is a [zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
/// implementation of binary/unary trees which represent logical formulae. This means that a `Formula` is not
/// just a tree, but a tree *and a particular location in that tree*, represented by the `zipper`; please read
/// the source material for more if you want to understand how this works. At the basic level you may assume that
/// `.*_unzip`, methods go 'down' the tree while `.*_zip` methods go up. If the formula is fully zipped (i.e. by calling
/// [`top_zip`]) the tree is in fact the whole formula, with `Zipper::Top` stored as a sentinel zipper.
///
/// The implementation therefore always operates/mutates the formula at its current location in the tree, so if you want
/// to operate on the very 'top' of the formula you must call [`top_zip`] first. The only exception to this is methods that
/// start with `.top_`, which you may assume call [`top_zip`] before performing mutations.
///
/// The mutating methods therefore change different properties about the formula but leave the 'location' of the formula,
/// i.e. the current `.tree`, untouched or as untouched as possible, so that these mutations can be done repeatedly as part
/// of, for example, an [`inorder_traverse_mut`]. It can be hard to understand this abstractly so it's best to read the
/// documentation for concrete transforming methods like [`combine`] or [`distribute_right`] for a clearer view.
///
/// This implementation does *not* use the builder pattern, both out of dislike but also because this would be impossible
/// to PyBind if methods took self or require a duplicate implementation.
///
/// For builder-style method chaining use the wonderful [`cascade!`](https://docs.rs/cascade/latest/cascade/) macro.
/// The Python wrappers do use a chainable pattern (taking and receiving `&mut self`) and can of course be used
/// from the Rust API, but the Python API is really designed for Pythonic use and this API for Rust.
///
/// Finally, the API is designed for safety in the sense that transformations that are applied are only applied
/// if they're valid at the current location in the formula and don't do anything otherwise! This is an opinionated
/// design decision because this crate is fundamentally designed for situations that demand lots of rapid transformation
/// the circumstance when you want to apply a transformation if possible or not do anything, e.g. when converting a formula
/// to conjunctive normal form. Returning [`Result<T, E>`] in this situation would inappropriately stop such fast transformation
/// chains and require a lot of branching in code. Nonetheless a trait may be added and implemented for this type which
/// reimplements the methods to allow one to immediately discern if a formula has changed after a method call because there's
/// no signal something has either happened or failed to (e.g. as would be signaled by returning a `Result<T, E>` type).
///
/// [`Result<T, E>`]: std::result::Result
/// [`top_zip`]: Self::top_zip
/// [`combine`]: Self::combine
/// [`distribute_right`]: Self::distribute_right
/// [`inorder_traverse_mut`]: Self::inorder_traverse_mut
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

/// This first impl block is for initialization and traversal.
impl<B: Symbolic, U: Symbolic, A: Symbolic> Formula<B, U, A> {
    /// A new formula that's just an atom.
    pub fn new(atom: A) -> Self {
        Formula {
            tree: Tree::Atom(atom),
            zipper: Zipper::Top,
        }
    }

    /// Traverse the current zipper one step, reconstructing the tree,
    /// whether you're in a binary tree (i.e. a left or right zipper)
    /// or a unary one (up zipper); doesn't do anything if you're at `Zipper::Top`.
    pub fn zip(&mut self) {
        match &self.zipper {
            Zipper::Top => {}
            Zipper::Right { .. } => self.zip_right(),
            Zipper::Left { .. } => self.zip_left(),
            Zipper::Up { .. } => self.zip_up(),
        }
    }

    /// In the subtree of a unary tree, go UP and eat the unary operator back into the tree. That is,
    /// ```text
    ///   zipper: (U, Zipper)       zipper: (*Zipper)
    ///                                
    ///            ^        =>         ^
    ///            |                   |
    ///                                
    ///           tree              tree: U
    ///                                   |
    ///                                old_tree
    /// ```
    pub fn zip_up(&mut self) {
        if let Zipper::Up { un, zip } = &mut self.zipper {
            self.tree.unify(*un);
            self.zipper = std::mem::take((*zip).as_mut());
        }
    }

    /// In the left subtree of a binary tree, go "right" and eat the binary operator and right subtree!
    /// You're basically recombining them, like
    /// ```text
    ///         zipper: (B, RTree, Zipper)        zipper: (*Zipper)
    ///          /                                      |
    ///         /                            =>         |
    ///       tree: LTree                          tree: B
    ///                                                /   \
    ///                                             LTree  RTree
    /// ```
    pub fn zip_right(&mut self) {
        if let Zipper::Right { sub, bin, zip } = &mut self.zipper {
            self.tree.combine(*bin, std::mem::take(sub));
            self.zipper = std::mem::take((*zip).as_mut());
        }
    }

    /// Just like [`zip_right`] except you're in the right subtree of a
    /// binary tree. The destination state is the exact same.
    ///
    /// [`zip_right`]: Self::zip_right
    pub fn zip_left(&mut self) {
        if let Zipper::Left { sub, bin, zip } = &mut self.zipper {
            self.tree.left_combine(*bin, std::mem::take(sub));
            self.zipper = std::mem::take((*zip).as_mut());
        }
    }

    /// The inverse of [`zip_left`]. Decompose a binary tree and
    /// traverse to the right subtree of a binary formula.
    ///
    /// [`zip_left`]: Self::zip_left
    pub fn unzip_right(&mut self) {
        if let Tree::Binary { conn, left, right } = &mut self.tree {
            self.zipper = Zipper::Left {
                bin: *conn,
                sub: std::mem::take((*left).as_mut()),
                zip: Box::new(std::mem::take(&mut self.zipper)),
            };
            self.tree = std::mem::take((*right).as_mut());
        }
    }

    /// The inverse of [`zip_right`]: decompose a binary tree and travel
    /// to the left subtree of a binary formula.
    ///
    /// [`zip_right`]: Self::zip_right
    pub fn unzip_left(&mut self) {
        if let Tree::Binary { conn, left, right } = &mut self.tree {
            self.zipper = Zipper::Right {
                bin: *conn,
                sub: std::mem::take((*right).as_mut()),
                zip: Box::new(std::mem::take(&mut self.zipper)),
            };
            self.tree = std::mem::take((*left).as_mut());
        }
    }

    /// Traverse to the formula contained in a unary tree.
    /// The inverse of [`zip_up`].
    ///
    /// [`zip_up`]: Self::zip_up
    pub fn unzip_down(&mut self) {
        if let Tree::Unary { conn, next } = &mut self.tree {
            self.zipper = Zipper::Up {
                un: *conn,
                zip: Box::new(std::mem::take(&mut self.zipper)),
            };
            self.tree = std::mem::take((*next).as_mut());
        }
    }

    /// Unzip the formula, i.e. return to the top node.
    /// After this, `self.tree` contains the whole formula.
    pub fn top_zip(&mut self) {
        while !self.zipper.is_top() {
            self.zip();
        }
    }

    /// Inorder traversal starting at the current context.
    /// If you want the whole formula simply [`top_zip`] first.
    /// Takes in a closure which can mutate the formula in
    /// place somehow. Returns Option<()> so traversal can be
    /// stopped early if needed; in most cases can be ignored.
    ///
    /// [`top_zip`]: Self::top_zip
    pub fn inorder_traverse_mut<F: FnMut(&mut Self) -> Option<()>>(
        &mut self,
        func: &mut F,
    ) -> Option<()> {
        match &self.tree {
            Tree::Binary { .. } => cascade! {
                self;
                ..unzip_left();
                ..inorder_traverse_mut(func);
                ..zip();    // a general zip to account for any potential transformations
                ..apply_mut(func)?;
                ..unzip_right();
                ..inorder_traverse_mut(func);
                ..zip()    // a general zip to account for any potential transformations
            },
            Tree::Unary { .. } => cascade! {
                self;
                ..apply_mut(func)?;
                ..unzip_down();
                ..inorder_traverse_mut(func);
                ..zip()    // a general zip to account for any potential transformations
            },
            Tree::Atom(_) => self.apply_mut(func)?,
        }
        Some(())
    }

    /// Preorder traversal starting at the current context.
    /// Also takes in a closure that can mutate the formula.
    /// This is a much more dangerous traversal strategy to call
    /// with a transformation method like [`rotate_right`] compared
    /// to [`inorder_traverse_mut`]; the latter will travel to the leaves
    /// of a formula and then perform transforms going back *up* the zipper,
    /// whereas this method will apply transformations immediately as it
    /// visits each node, having potentially weird consequences if there
    /// are in place mutations going on with formulae.
    ///
    /// [`rotate_right`]: Self::rotate_right
    /// [`inorder_traverse_mut`]: Self::inorder_traverse_mut
    pub fn preorder_traverse_mut<F: FnMut(&mut Self) -> Option<()>>(
        &mut self,
        func: &mut F,
    ) -> Option<()> {
        self.apply_mut(func)?;
        match &self.tree {
            Tree::Binary { .. } => cascade! {
                self;
                ..apply_mut(func)?;
                ..unzip_left();
                ..preorder_traverse_mut(func);
                ..zip();    // a general zip to account for any potential transformations
                ..unzip_right();
                ..preorder_traverse_mut(func);
                ..zip()    // a general zip to account for any potential transformations
            },
            Tree::Unary { .. } => cascade! {
                self;
                ..apply_mut(func)?;
                ..unzip_down();
                ..preorder_traverse_mut(func);
                ..zip()    // a general zip to account for any potential transformations
            },
            Tree::Atom(_) => self.apply_mut(func)?,
        }
        Some(())
    }

    pub fn inorder_traverse<F: FnMut(&Tree<B, U, A>) -> Option<()>>(
        &self,
        func: &mut F,
    ) -> Option<()> {
        Some(self.tree.inorder_traverse(func)?)
    }

    pub fn preorder_traverse<F: FnMut(&Tree<B, U, A>) -> Option<()>>(
        &self,
        func: &mut F,
    ) -> Option<()> {
        Some(self.tree.preorder_traverse(func)?)
    }

    /// Purely for the sake of nicer syntax, allows closures to be called method-style
    /// as part of method chaining.
    fn apply_mut<F: FnMut(&mut Self) -> Option<()>>(&mut self, func: &mut F) -> Option<()> {
        Some(func(self)?)
    }

    /// Purely for the sake of nicer syntax, allows closures to be called method-style
    /// as part of method chaining.
    fn apply<F: FnMut(&Self) -> Option<()>>(&self, func: &mut F) -> Option<()> {
        Some(func(self)?)
    }
}

/// This impl is about manipulating and combining formulas.
impl<B: Symbolic, U: Symbolic, A: Symbolic> Formula<B, U, A> {
    /// Connects to a new formula WITHOUT
    /// unzipping, which is why this takes in a tree.
    /// Whatever the current `.tree` is will become
    /// the left subtree of a binary tree, where `new_tree`
    /// is the right subtree and they're connected by `bin`.
    /// This is a very zipper-style impl which might be counter
    /// intuitive, and perhaps better illustrated with some
    /// poor ASCII art:
    ///
    /// ```text
    ///          zipper                 zipper: Zipper::Right(bin, new_tree, old_zipper)
    ///                                
    ///            ^        =>            /
    ///            |                    /
    ///                                
    ///           tree               tree
    /// ```
    pub fn combine(&mut self, bin: B, new_tree: Tree<B, U, A>) {
        self.zipper = Zipper::Right {
            bin,
            sub: new_tree,
            zip: Box::new(std::mem::take(&mut self.zipper)),
        };
    }

    /// Exactly the same as [`combine`] but the new subtree is inserted as
    /// a left subtree, so you're now in the right subtree of a binary tree.
    /// And therefore you end up with a [`Zipper::Left`].
    ///
    /// [`combine`]: Self::combine
    /// [`Zipper::Left`]: Zipper::Left
    pub fn left_combine(&mut self, bin: B, new_tree: Tree<B, U, A>) {
        self.zipper = Zipper::Left {
            bin,
            sub: new_tree,
            zip: Box::new(std::mem::take(&mut self.zipper)),
        };
    }

    /// Combine two formulas with a binary connective.
    /// But unzip first.
    pub fn top_combine(&mut self, bin: B, mut formula: Self) {
        formula.top_zip();
        self.top_zip();
        self.combine(bin, formula.tree);
    }

    /// [`top_combine`] but on the left side.
    ///
    /// [`top_combine`]: Self::top_combine
    pub fn top_left_combine(&mut self, bin: B, mut formula: Self) {
        formula.top_zip();
        self.top_zip();
        self.left_combine(bin, formula.tree)
    }

    /// Insert a unary operator in the formula's current position.
    pub fn unify(&mut self, un: U) {
        self.zipper = Zipper::Up {
            un,
            zip: Box::new(std::mem::take(&mut self.zipper)),
        };
    }

    /// Insert a unary operator for the whole formula.
    pub fn top_unify(&mut self, un: U) {
        cascade! {
            self;
            ..top_zip();
            ..unify(un)
        }
    }

    /// A function which demonstrates some zipper-y fun, if you're currently at the
    /// right or left subtree of a binary formula, i.e. the current zipper is
    /// `Zipper::Right{..}` or `Zipper::Left{..}`, swap your position with the other
    /// subtree (without moving memory). Otherwise, the formula remains the same.
    pub fn flip(&mut self) {
        self.zipper.flip()
    }

    /// If it applies in the current context, 'rotate' a tree formula,
    /// i.e. change precedence between two binary operators,
    /// to the left. As an example,
    /// ```text
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
    /// ```
    /// is an example of a left rotation.
    /// Rotations are always performed assuming the current zipper holds the
    /// lower-precedence `B`, i.e. the one higher up in the tree. In the example
    /// above, the rotation would be performed on a `Formula` where the `zipper`
    /// is the \\ pictured, holding the → operator. `self` is left in the same
    /// position after rotation, with // denoting the new active zipper.
    pub fn rotate_left(&mut self) {
        if let Formula {
            tree:
                Tree::Binary {
                    conn,
                    left: b,
                    right: c,
                },
            zipper: Zipper::Left { bin, sub: a, .. },
        } = self
        {
            std::mem::swap(conn, bin);
            std::mem::swap(b.as_mut(), c.as_mut());
            std::mem::swap(a, b.as_mut()); // b now holds the c tree, so really swapping A, C
        }
        // You now need a right zipper, so flip it!
        self.flip()
    }

    /// If it applies in the current context, 'rotate' a tree formula,
    /// i.e. change precedence between two binary operators,
    /// to the right. As an example,
    /// ```text
    ///          ∧                               
    ///       //   \
    ///      →      C   
    ///    /   \
    ///  A       B
    ///
    ///               =>
    ///
    ///                     →                                       
    ///                   /   \\
    ///                 A       ∧          
    ///                       /   \
    ///                     B       C
    /// ```
    /// is an example of a right rotation. More detail available in the
    /// documentation of [`.rotate_left()`].
    ///
    /// [`.rotate_left()`]: Self::rotate_left
    pub fn rotate_right(&mut self) {
        if let Formula {
            tree:
                Tree::Binary {
                    conn,
                    left: a,
                    right: b,
                },
            zipper: Zipper::Right { bin, sub: c, .. },
        } = self
        {
            std::mem::swap(conn, bin);
            std::mem::swap(a.as_mut(), b.as_mut());
            std::mem::swap(c, b.as_mut()); // b now holds the a tree, so really swapping A, C
        }
        // You now need a left zipper, so flip it!
        self.flip()
    }

    /// 'Distribute' a binary operator over the right (binary) subtree.
    /// Often used in, for example, creating the conjunctive normal forms
    /// of formulae. Easiest to see by example:
    /// ```text
    ///         ∧
    ///      /     \\
    ///    p        ∨
    ///          /      \
    ///        q         r
    ///
    ///                        =>
    ///                                    ∨
    ///                               /        \\
    ///                            ∧               ∧
    ///                        /       \       /       \
    ///                       p         q     p         r
    /// ```
    /// The dummy formula corresponding to `p` above gets cloned.
    /// Just like with the rotation methods, the above method occurs
    /// starting from the higher-precedence operator (lower in the subtree)
    /// corresponding to \\ being the active zipper, and \\ in the second
    /// formula describes the active zipper after distribution.
    pub fn distribute_right(&mut self) {
        if !self.zipper.is_left() || !self.tree.is_binary() {
            return;
        }
        cascade! {
            let curr = self;
            ..rotate_left();
            // while you're here, steal another copy of the formula to distribute
            let (clone, bin) = if let &Tree::Binary {ref left, conn,.. } = &curr.tree {(left.as_ref().clone(), conn)}
                               else {unreachable!()};
            ..zip_right();
            ..unzip_right();
            ..left_combine(bin, clone)
        }
    }

    /// Distribute a binary operator over the left subtree (corresponding to a right
    /// rotation). See [`distribute_right`] for more.
    ///
    /// [`distribute_right`]: Self::distribute_right
    pub fn distribute_left(&mut self) {
        if !self.zipper.is_right() || !self.tree.is_binary() {
            return;
        }
        cascade! {
            let curr = self;
            ..rotate_right();
            // while you're here, steal another copy of the formula to distribute
            let (clone, bin) = if let &Tree::Binary {ref right, conn,.. } = &curr.tree {(right.as_ref().clone(), conn)}
                               else {unreachable!()};
            ..zip_left();
            ..unzip_left();
            ..combine(bin, clone)
        }
    }

    /// Very similar to the [`.rotate_*`] methods but with unary operators
    /// swapping precedence with binary ones instead. Because of this the
    /// `.lower_*` methods don't reverse each other unlike the [`.rotate_*`] methods.
    /// This method unifies the right subformula.
    ///
    /// [`.rotate_*`]: Self::rotate_right
    pub fn lower_right(&mut self) {
        if let Formula {
            tree: Tree::Binary { right, .. },
            zipper: Zipper::Up { un, zip },
        } = self
        {
            right.as_mut().unify(*un);
            self.zipper = *std::mem::take(zip);
        }
    }

    /// Same as [`.lower_right()`] but unifies the left subformula.
    ///
    /// [`.lower_right()`]: Self::lower_right
    pub fn lower_left(&mut self) {
        if let Formula {
            tree: Tree::Binary { left, .. },
            zipper: Zipper::Up { un, zip },
        } = self
        {
            left.as_mut().unify(*un);
            self.zipper = *std::mem::take(zip);
        }
    }

    /// Distribute a unary operator over a binary operator. Optionally
    /// pass in a new binary operator to root the formula after distribution.
    /// Basically like executing [`lower_left`] and [`lower_right`]
    /// at the same time, and optionally swapping the root of the binary tree.
    /// If you're unfamiliar with logic, this method exists because in numerous
    /// languages a binary operator will have a 'dual' operator that it swaps
    /// with when a unary operator is distributed over the two operands.
    ///
    /// [`lower_left`]: Self::lower_left
    /// [`lower_right`]: Self::lower_right
    pub fn distribute_down(&mut self, new_bin: Option<B>) {
        if let Formula {
            tree: Tree::Binary { conn, left, right },
            zipper: Zipper::Up { un, zip },
        } = self
        {
            left.unify(*un);
            right.unify(*un);
            if let Some(mut b) = new_bin {
                std::mem::swap(&mut b, conn)
            }
            self.zipper = *std::mem::take(zip);
        }
    }

    /// Swap two unary operators, optionally changing the one that's been
    /// swapped up. This exists because oftentimes unary operators have
    /// a 'dual' operator which they change to when another unary operator
    /// changes precedence with them.
    pub fn push_down(&mut self, new_un: Option<U>) {
        if let Formula {
            tree: Tree::Unary { conn, .. },
            zipper: Zipper::Up { un, .. },
        } = self
        {
            if let Some(mut u) = new_un {
                std::mem::swap(&mut u, conn)
            }
            std::mem::swap(conn, un)
        }
    }

    /// Instantiate an *atom* in the formula (as usual, starting where you currently are)
    /// with another tree subformula. If you want to do this over a whole formula,
    /// just call this inside [`inorder_traverse_mut`].
    ///
    /// [`inorder_traverse_mut`]: Self::inorder_traverse_mut
    pub fn instantiate(&mut self, formulas: &HashMap<A, Tree<B, U, A>>) {
        if let Tree::Atom(a) = self.tree {
            if formulas.contains_key(&a) {
                self.tree = formulas[&a].clone()
            }
        }
    }
}

/// This block is about interfacing with tensors.
impl<B: Symbolic, U: Symbolic, A: Symbolic> Formula<B, U, A> {
    /// Given some arbitrary mapping from symbols to nonnegative integers, encode
    /// a formula as a list of integers corresponding to an inorder traversal of
    /// the nodes, and another list of the parent-child relationships between
    /// the nodes. If the mapping given is not total, return None.
    /// The edges are returned in COO format (two lists of pairs,
    /// corresponding by index).
    pub fn tensorize(
        &self,
        mapping: &HashMap<Symbol<B, U, A>, usize>,
    ) -> Result<(Vec<usize>, Vec<Vec<usize>>), ParseError> {
        let mut nodes: Vec<usize> = vec![];
        let mut edges: Vec<Vec<usize>> = vec![vec![], vec![]];

        let mut counter: usize = 0;
        let mut stack: Vec<usize> = vec![];

        self.preorder_traverse(&mut |node| {
            nodes.push(mapping[&Symbol::from_tree(&node)]);
            if let Some(idx) = stack.pop() {
                edges[0].push(idx);
                edges[1].push(counter);
            }
            if let Tree::Binary { .. } | Tree::Unary { .. } = node {
                stack.push(counter)
            }
            if let Tree::Binary { .. } = node {
                stack.push(counter)
            }
            Some(counter += 1)
        })
        .ok_or(ParseError::IncompleteEnum)?;

        Ok((nodes, edges))
    }

    /// Store all the atoms that appear in the formula in a HashSet.
    pub fn get_atoms(&self) -> HashSet<A> {
        let mut atoms: HashSet<A> = HashSet::new();
        self.tree.inorder_traverse(&mut |t: &Tree<B, U, A>| {
            Some(if let &Tree::Atom(a) = t {
                atoms.insert(a);
            })
        });
        atoms
    }

    /// Normalize all the atoms in a formula by replacing its atoms with atoms
    /// that index their first appearance in an inorder traversal.
    /// The indices given are taken from a passed in iterator (over A) on which
    /// [`std::iter::Iterator::next`] is called precisely once every time a
    /// new atom is observed. In case the iterator runs out, returns None.
    pub fn normalize<I: Iterator<Item = A>>(&self, mut indices: I) -> Option<Self> {
        let mut seen: HashMap<A, A> = HashMap::new();
        let mut formula = self.clone();
        formula.inorder_traverse_mut(&mut |node| {
            Some(if let Tree::Atom(a) = node.tree {
                node.tree = Tree::Atom(*seen.entry(a).or_insert(indices.next()?));
            })
        })?;
        Some(formula)
    }
}

// Special functions for enumerable operator types. Utilities for a rainy day.
impl<B, U, A> Formula<B, U, A>
where
    B: Symbolic + Sequence,
    U: Symbolic + Sequence,
    A: Symbolic,
{
    /// Give the number of binary operators in this formula type.
    fn num_binary() -> usize {
        enum_iterator::cardinality::<B>()
    }
    /// Give the number of unary operators in this formula type.
    fn num_unary() -> usize {
        enum_iterator::cardinality::<U>()
    }
    /// Offer an indexing map for all unary operators.
    fn unary_counting(offset: usize) -> HashMap<U, usize> {
        let mut counter = offset..;
        all::<U>()
            .map(|s| (s, counter.next().unwrap()))
            .collect::<HashMap<U, usize>>()
    }

    /// Offer an indexing map for the string repr of the unary operators.
    fn unary_str_counting(offset: usize) -> HashMap<String, usize> {
        let mut counter = offset..;
        all::<U>()
            .map(|s| (s.to_string(), counter.next().unwrap()))
            .collect::<HashMap<String, usize>>()
    }

    /// Offer an indexing map for all binary operators.
    fn binary_counting(offset: usize) -> HashMap<B, usize> {
        let mut counter = offset..;
        all::<B>()
            .map(|s| (s, counter.next().unwrap()))
            .collect::<HashMap<B, usize>>()
    }

    /// Offer an indexing map for the string repr of the binary operators.
    fn binary_str_counting(offset: usize) -> HashMap<String, usize> {
        let mut counter = offset..;
        all::<B>()
            .map(|s| (s.to_string(), counter.next().unwrap()))
            .collect::<HashMap<String, usize>>()
    }

    /// Index all operators for the type, unary first, wrapped in the Symbol type.
    fn operator_counting(offset: usize) -> HashMap<Symbol<B, U, A>, usize> {
        Self::unary_counting(offset)
            .into_iter()
            .map(|(u, i)| (Symbol::Unary(u), i))
            .chain(
                Self::binary_counting(offset + cardinality::<U>())
                    .into_iter()
                    .map(|(b, i)| (Symbol::Binary(b), i)),
            )
            .collect::<HashMap<_, _>>()
    }

    /// Index all operators for the type, unary first, wrapped in the Symbol type.
    fn operator_str_counting(offset: usize) -> HashMap<String, usize> {
        Self::unary_str_counting(offset)
            .into_iter()
            .chain(Self::binary_str_counting(offset + cardinality::<U>()).into_iter())
            .collect::<HashMap<_, _>>()
    }
}

impl<B, U, A> Display for Formula<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    /// Read string representation starting from current position in formula.
    /// THIS WILL NOT PRINT ANY UNZIPPED PARTS OF THE FORMULA. Make sure to
    /// [`Formula::top_zip`] first if you want the whole formula.
    /// Printing is an easy way to see "where" you are in the formula.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut outparam = String::new();
        self.tree.read_inorder(&mut outparam);
        write!(f, "{}", outparam)
    }
}

impl<B, U, A> From<Tree<B, U, A>> for Formula<B, U, A>
where
    B: Symbolic,
    U: Symbolic,
    A: Symbolic,
{
    fn from(value: Tree<B, U, A>) -> Self {
        Formula {
            tree: value,
            zipper: Zipper::Top,
        }
    }
}
