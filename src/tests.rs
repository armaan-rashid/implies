use crate::prop::Proposition;
#[cfg(tests)]
use std::collections::HashMap;

#[test]
fn inorder_sequence() -> Result<(), ValidationError> {
    let mut formula = PropFormula::new('p');
    assert_eq!("p", formula.to_string());
    formula = formula.negate();
    assert_eq!("¬p", formula.to_string());
    formula = formula.combine(BinaryOp::Iff, PropFormula::new('q'));
    formula.validate()?;
    assert_eq!(
        vec![
            PropSymbol::Unary(UnaryOp::Not),
            PropSymbol::Atom('p'),
            PropSymbol::Binary(BinaryOp::Iff),
            PropSymbol::Atom('q')
        ],
        formula.to_inorder_sequence()
    );
    assert_eq!("¬p ↔ q", formula.to_string());
    // let's do a more complicated precedence example
    let test = PropFormula::from_string("(p → q) → (r ∧ s)")?;
    assert_eq!("(p → q) → r ∧ s", test.to_string()); // we're assuming that to_string() does least parentheses possible
    Ok(())
}

#[test]
fn preorder_sequence() -> Result<(), ValidationError> {
    let mut formula = PropFormula::new('p');
    assert_eq!("p", formula.to_preorder_string());
    formula = formula.negate();
    assert_eq!(
        vec![PropSymbol::Unary(UnaryOp::Not), PropSymbol::Atom('p')],
        formula.to_preorder_sequence()
    );
    assert_eq!("¬p", formula.to_preorder_string());
    formula = formula.combine(BinaryOp::Iff, PropFormula::new('q'));
    formula.validate()?;
    assert_eq!("¬p ↔ q", formula.to_string());
    assert_eq!(
        vec![
            PropSymbol::Binary(BinaryOp::Iff),
            PropSymbol::Unary(UnaryOp::Not),
            PropSymbol::Atom('p'),
            PropSymbol::Atom('q')
        ],
        formula.to_preorder_sequence()
    );
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
    assert_ne!(
        from_str,
        PropFormula::new('p')
            .combine(BinaryOp::Iff, PropFormula::new('q'))
            .negate()
    );
    // let's try associativity!
    let mut precedence = PropFormula::from_string("p → q → r")?;
    let right = PropFormula::new('p').combine(
        BinaryOp::Implies,
        PropFormula::new('q').combine(BinaryOp::Implies, PropFormula::new('r')),
    );
    let wrong = PropFormula::new('p')
        .combine(BinaryOp::Implies, PropFormula::new('q'))
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
    assert_eq!(
        PropFormula::from_string("(A → (B → C)) → (A → B) → B → C")?,
        complex
    );
    assert_eq!(
        PropFormula::from_string("(A → (B → C)) → ((A → B) → B → C)")?,
        complex
    );
    assert_eq!(
        PropFormula::from_string("(A → (B → C)) → ((A → B) → (B → C))")?,
        complex
    );
    assert_eq!(
        PropFormula::from_string("((A → (B → C)) → ((A → B) → (B → C)))")?,
        complex
    );
    // finally repeated negation!
    assert_eq!(
        PropFormula::from_string("¬¬¬d")?,
        PropFormula::new('d').negate().negate().negate()
    );
    for _ in 1..=75 {
        // "robust" testing against whatever wacky formulas generate() makes
        let random = PropFormula::random_generate(Some(75), None);
        assert_eq!(PropFormula::from_string(&random.to_string())?, random);
    }
    Ok(())
}

#[test]
fn instantiation() -> Result<(), ValidationError> {
    let mut axiom = PropFormula::distribution();
    assert_eq!(
        PropFormula::from_string("(A → (B → C)) → (A → B) → B → C")?,
        axiom
    );
    let mut atomic_formulas = HashMap::new();
    atomic_formulas.insert('A', PropFormula::from_string("p → q → r")?);
    axiom = axiom.instantiate(atomic_formulas.clone());
    assert_eq!(
        PropFormula::from_string("((p → q → r) → (B → C)) → ((p → q → r) → B) → B → C")?,
        axiom
    );
    atomic_formulas.insert('B', PropFormula::from_string("f ∧ g → h")?);
    axiom = axiom.instantiate(atomic_formulas.clone());
    assert_eq!(
        PropFormula::from_string(
            "((p → q → r) → ((f ∧ g → h) → C)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → C"
        )?,
        axiom
    );
    atomic_formulas.insert('C', PropFormula::from_string("¬¬¬d")?);
    axiom = axiom.instantiate(atomic_formulas.clone());
    assert_eq!(PropFormula::from_string("((p → q → r) → ((f ∧ g → h) → ¬¬¬d)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → ¬¬¬d")?, axiom);
    Ok(())
}

#[test]
fn check_instancing() -> Result<(), ValidationError> {
    assert!(PropFormula::from_string(
        "((p → q → r) → ((f ∧ g → h) → ¬¬¬d)) → ((p → q → r) → (f ∧ g → h)) → (f ∧ g → h) → ¬¬¬d"
    )?
    .is_instance(&PropFormula::distribution()));
    assert!(!PropFormula::from_string("(p → q → r)")?.is_instance(&PropFormula::symmetry()));
    assert!(
        PropFormula::from_string("(p → q → r) → (p → q → r) → (p → q → r)")?
            .is_instance(&PropFormula::symmetry())
    );
    assert!(PropFormula::from_string("((p → q) → (¬q → ¬p))")?
        .is_instance(&PropFormula::contraposition()));
    assert!(
        !PropFormula::from_string("(p → q → ¬q → ¬p)")?.is_instance(&PropFormula::contraposition())
    );
    Ok(())
}

#[test]
fn validate_generate() -> Result<(), ValidationError> {
    for i in 0..75 {
        let formula = PropFormula::random_generate(Some(50), None);
        formula.validate()?;
        if i % 25 == 0 {
            println!("{}", formula.to_string())
        }
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
    assert_eq!(
        PropFormula::largest_common_subformula(&first, &second).unwrap(),
        1
    );
    assert_eq!(
        PropFormula::largest_common_subformula(&first, &third).unwrap(),
        3
    );
    assert_eq!(
        PropFormula::largest_common_subformula(&second, &third).unwrap(),
        1
    );
    Ok(())
}

#[test]
fn mp_testing() -> Result<(), MPErr> {
    let proof: Proof = vec![
        PropFormula::from_string("(p → q) → r").unwrap(),
        PropFormula::from_string("p → q → r").unwrap(),
    ];
    PropFormula::from_string("p")
        .unwrap()
        .try_modus_ponens(&proof)?;
    PropFormula::from_string("p → q")
        .unwrap()
        .try_modus_ponens(&proof)?;
    assert!(PropFormula::from_string("q → r")
        .unwrap()
        .try_modus_ponens(&proof)
        .is_err());
    Ok(())
}
