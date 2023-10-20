use crate::formula::{Formula, Tree, Zipper};
use crate::prop::{Atom, PropBinary, PropFormula, PropUnary, Proposition};
use crate::symbol::{Match, ParseError, ParsedSymbols, Symbol};
use cascade::cascade;
use pyo3::PyResult;
use std::collections::HashMap;

type PropSymbol = Symbol<PropBinary, PropUnary, Atom>;

#[test]
fn atomic_matching() -> Result<(), ParseError> {
    if let Some(atom) = Atom::get_match("a") {
        assert_eq!(atom, Atom(0));
        assert_eq!(Atom::match_prefix("a").unwrap(), (1, Atom(0)));
        // try to match on the symbol variant now
        assert!(PropSymbol::get_match("a").is_some());
        assert_eq!(Symbol::Atom(Atom(0)), PropSymbol::get_match("a").unwrap());
        assert_eq!(
            (1, Symbol::Atom(Atom(0))),
            PropSymbol::match_prefix("a").unwrap()
        );
        Ok(())
    } else {
        Err(ParseError::InvalidStr("a".to_string()))
    }
}

#[test]
fn symbol_parse() -> Result<(), ParseError> {
    let syms1: Vec<Symbol<PropBinary, PropUnary, Atom>> = vec![
        Symbol::Atom(Atom(0)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Atom(Atom(1)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Atom(Atom(2)),
    ];
    assert_eq!(syms1, ParsedSymbols::from("a -> b -> c").0?);
    let syms2: Vec<Symbol<PropBinary, PropUnary, Atom>> = vec![
        Symbol::Left,
        Symbol::Atom(Atom(0)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Atom(Atom(1)),
        Symbol::Right,
        Symbol::Binary(PropBinary::Implies),
        Symbol::Atom(Atom(2)),
    ];
    assert_ne!(syms1, syms2);
    assert_eq!(syms2, ParsedSymbols::from("(a -> b) -> c").0?);
    let mut syms: Vec<PropSymbol> = vec![Symbol::Atom(Atom(0))];
    let mut parsed: Vec<PropSymbol> = ParsedSymbols::from("a").0?;
    assert_eq!(syms, parsed);

    // a sampling of some of the weird symbol representations
    syms = vec![Symbol::Binary(PropBinary::Implies)];
    parsed = ParsedSymbols::from("->").0?;
    let mut parsed1: Vec<PropSymbol> = ParsedSymbols::from("implies").0?;
    assert_eq!(syms, parsed);
    assert_eq!(syms, parsed1);

    syms = vec![Symbol::Unary(PropUnary::Not)];
    parsed = ParsedSymbols::from("not").0?;
    assert_eq!(syms, parsed);

    syms = vec![Symbol::Binary(PropBinary::Or)];
    parsed = ParsedSymbols::from("\\/").0?;
    assert_eq!(syms, parsed);

    // technically you should be allowed to put in formulas without spaces,
    // in case you're weird
    syms = vec![
        Symbol::Unary(PropUnary::Not),
        Symbol::Atom(Atom(0)),
        Symbol::Binary(PropBinary::Or),
        Symbol::Atom(Atom(1)),
        Symbol::Binary(PropBinary::Iff),
        Symbol::Atom(Atom(0)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Atom(Atom(1)),
    ];

    assert_eq!(
        ParsedSymbols::<PropBinary, PropUnary, Atom>::from("notaorbiffaimpliesb").0?,
        syms
    );
    assert_eq!(
        ParsedSymbols::<PropBinary, PropUnary, Atom>::from("¬a∨b↔a→b").0?,
        syms
    );

    Ok(())
}

#[test]
fn parentheses_parse() -> Result<(), ParseError> {
    // try a bunch of weird but still correctly formed
    // strings w parens in em
    assert!(PropFormula::from_str("").is_err_and(|e| e == ParseError::EmptyFormula));
    let mut _f = PropFormula::from_str("(((((a implies b)))))")?;
    _f = PropFormula::from_str("(a implies ((b implies c)))")?;
    _f = PropFormula::from_str("(a or b)")?;
    _f = PropFormula::from_str("(a or b) and (a or c)")?;
    assert!(PropFormula::from_str("(a implies (b implies c)))")
        .is_err_and(|e| e == ParseError::UnbalancedParentheses));
    assert!(PropFormula::from_str("a and b () implies c")
        .is_err_and(|e| e == ParseError::NotAtomic("(".to_string())));
    assert!(PropFormula::from_str("a implies not").is_err());
    Ok(())
}

#[test]
fn zip_zip() -> Result<(), ParseError> {
    let formula = cascade! {
        let f = PropFormula::new(Atom(0));
        ..combine(PropBinary::Implies, f.clone().tree);
        let f2 = cascade! {f.clone(); ..zip_right(); ..unzip_right();};
        assert_ne!(f, f2);    // zipping should have done something
        assert_eq!(f.zipper, Zipper::Right { bin: PropBinary::Implies, sub: f.tree.clone(), zip: Box::new(Zipper::Top) });
        assert_eq!(f2.zipper, Zipper::Left { bin: PropBinary::Implies, sub: f2.tree.clone(), zip: Box::new(Zipper::Top) });
        assert_eq!(f, cascade! {
            f2;
            ..zip_left();
            ..unzip_left();
        });
    };
    // let's add a unary op and try a zip
    cascade! {
        let formula = formula;
        ..unify(PropUnary::Not);
        // formula should be (not a) -> a at this point
        let f2 = cascade! {formula.clone(); ..zip_up();};
        assert_ne!(formula, f2);    // zipping up should do something
        assert_eq!(formula, cascade!{f2; ..unzip_down();});
    };
    Ok(())
}

#[test]
fn formula_parse() -> Result<(), ParseError> {
    // i.e. we want to parse (a -> not b) <-> (b -> not a)
    let syms: Vec<PropSymbol> = vec![
        Symbol::Atom(Atom(0)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Unary(PropUnary::Not),
        Symbol::Atom(Atom(1)),
        Symbol::Binary(PropBinary::Iff),
        Symbol::Atom(Atom(1)),
        Symbol::Binary(PropBinary::Implies),
        Symbol::Unary(PropUnary::Not),
        Symbol::Atom(Atom(0)),
    ];
    let parsed: Vec<PropSymbol> = ParsedSymbols::from("a -> not b <-> b -> not a").0?;
    assert_eq!(parsed, syms);
    assert_eq!(
        Symbol::main_operator(&parsed[..])?,
        (4, Symbol::Binary(PropBinary::Iff))
    );
    assert_eq!(
        Symbol::main_operator(&parsed[..4])?,
        (1, Symbol::Binary(PropBinary::Implies))
    );
    assert_eq!(
        Symbol::main_operator(&parsed[5..])?,
        (1, Symbol::Binary(PropBinary::Implies))
    );
    let target = cascade! {
        let formula = PropFormula::new(Atom(0));
        ..combine(PropBinary::Implies, cascade!{let f = PropFormula::new(Atom(1)); ..unify(PropUnary::Not); ..top_zip(); f.tree});
        ..zip_right();
        ..combine(PropBinary::Iff, cascade!{
            let clone = formula.clone();
            ..unzip_left();
            ..tree = Tree::Atom(Atom(1));
            ..zip_right();
            ..unzip_right();
            ..unzip_down();
            ..tree = Tree::Atom(Atom(0));
            ..top_zip();
            clone.tree
        });
        ..zip_right();
    };
    assert_eq!(Tree::build_tree(&parsed[..])?, target.tree);
    Ok(())
}

#[test]
fn rotation_and_precedence() -> Result<(), ParseError> {
    let mut right = PropFormula::from_str("a -> b -> c")?;
    let mut left = PropFormula::from_str("(a -> b) -> c")?;
    assert_eq!(
        right,
        cascade! {
            PropFormula::new(Atom(0));
            ..combine(PropBinary::Implies, Tree::Atom(Atom(1)));
            ..zip_right();
            ..unzip_right();
            ..combine(PropBinary::Implies, Tree::Atom(Atom(2)));
            ..top_zip();
        }
    );
    assert_eq!(
        left,
        cascade! {
            PropFormula::new(Atom(0));
            ..combine(PropBinary::Implies, Tree::Atom(Atom(1)));
            ..zip_right();
            ..combine(PropBinary::Implies, Tree::Atom(Atom(2)));
            ..top_zip();
        }
    );
    assert_ne!(right, left);
    assert_eq!(
        right,
        cascade! {
            left;
            ..unzip_left();
            ..rotate_right();
            ..zip_left();
        }
    );
    // now try multiple operators of competing precedence
    left = PropFormula::from_str("a and b implies c")?;
    right = PropFormula::from_str("a and (b implies c)")?;
    assert_eq!(
        right,
        cascade! {
            PropFormula::new(Atom(0));
            ..combine(PropBinary::And, Tree::Atom(Atom(1)));
            ..zip_right();
            ..unzip_right();
            ..combine(PropBinary::Implies, Tree::Atom(Atom(2)));
            ..top_zip();
        }
    );
    assert_eq!(
        left,
        cascade! {
            PropFormula::new(Atom(0));
            ..combine(PropBinary::And, Tree::Atom(Atom(1)));
            ..zip_right();
            ..combine(PropBinary::Implies, Tree::Atom(Atom(2)));
            ..top_zip();
        }
    );
    assert_ne!(right, left);
    assert_eq!(
        right,
        cascade! {
            left;
            ..unzip_left();
            ..rotate_right();
            ..zip_left();
        }
    );
    Ok(())
}

#[test]
fn distribute_binary() -> Result<(), ParseError> {
    let mut f = PropFormula::from_str("a or (b and c)")?;
    let mut check = cascade! {
        PropFormula::new(Atom(0));
        ..combine(PropBinary::Or, Tree::Atom(Atom(1)));
        ..zip_right();
        ..unzip_right();
        ..combine(PropBinary::And, Tree::Atom(Atom(2)));
        ..top_zip();
    };
    // sanity check
    assert_eq!(f, check);
    f = cascade! {f; ..unzip_right(); ..distribute_right(); ..top_zip();};
    check = cascade! {
        PropFormula::new(Atom(0));
        ..combine(PropBinary::Or, Tree::Atom(Atom(1)));
        ..zip_right();
        ..combine(PropBinary::And, Tree::Atom(Atom(0)));
        ..zip_right();
        ..unzip_right();
        ..combine(PropBinary::Or, Tree::Atom(Atom(2)));
        ..top_zip();
    };
    assert_eq!(f, check);
    assert_eq!(f, PropFormula::from_str("(a or b) and (a or c)")?);
    f = cascade! {f; ..unzip_left(); ..distribute_left(); ..top_zip();};
    // at this point the formula should be (a and (a or c)) or (b and (a or c))
    assert_eq!(
        f,
        PropFormula::from_str("(a and (a or c)) or (b and (a or c))")?
    );
    Ok(())
}

#[test]
fn read_and_write() -> Result<(), ParseError> {
    let mut f = PropFormula::from_str("a -> (b implies c)")?;
    assert_eq!(f.to_string(), "a → b → c");
    f = PropFormula::from_str("(a implies b) implies c")?;
    assert_eq!(f.to_string(), "(a → b) → c");
    f = PropFormula::from_str("(a or (b and c)) implies (a or b)")?;
    assert_eq!(f.to_string(), "a ∨ b ∧ c → a ∨ b");
    Ok(())
}

#[test]
fn unary_over_binary() -> Result<(), ParseError> {
    let f1 = PropFormula::from_str("not (a -> b)")?;
    let f2 = PropFormula::from_str("not a -> b")?;
    assert_eq!(cascade! {f1.clone(); ..unzip_down(); ..lower_left();}, f2);
    assert_eq!(
        cascade! {f1.clone(); ..unzip_down(); ..lower_right();},
        PropFormula::from_str("a -> not b")?
    );
    let f3 = PropFormula::from_str("not a -> not b")?;
    assert_eq!(
        cascade! {f1.clone(); ..unzip_down(); ..distribute_down(None);},
        f3
    );
    let f3 = PropFormula::from_str("not a or not b")?;
    assert_eq!(
        cascade! {f1.clone(); ..unzip_down(); ..distribute_down(Some(PropBinary::Or));},
        f3
    );
    Ok(())
}

#[test]
fn instantiation() -> Result<(), ParseError> {
    let mut f = PropFormula::from_str("not a implies b implies c")?;
    let mut map: HashMap<Atom, Tree<PropBinary, PropUnary, Atom>> = HashMap::new();
    map.insert(Atom(0), f.tree.clone());
    cascade! {
        &mut f;
        ..inorder_traverse_mut(&mut |f| f.instantiate(&map));
        ..top_zip()
    }
    assert_eq!(
        PropFormula::from_str("not (not a implies b implies c) implies b implies c")?,
        f
    );

    cascade! {
        &mut f;
        ..unzip_right();
        ..unzip_right();
        ..tree = Tree::Atom(Atom(0));
        ..top_zip()
    }

    assert_eq!(
        PropFormula::from_str("not (not a implies b implies c) implies b implies a")?,
        f
    );

    f.inorder_traverse_mut(&mut |f| f.instantiate(&map));

    assert_eq!(PropFormula::from_str("not (not (not a implies b implies c) implies b implies c) implies b implies (not a implies b implies c)")?, f);

    Ok(())
}
