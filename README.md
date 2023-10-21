# implies: a Pybound Rust crate for logical formulas

**implies** is a Rust crate for storing logical formulas as parse trees and performing complex operations on them,
like substitution, rotation, conversion to conjunctive normal form, and more. Propositional logic comes pre-implemented, 
but this crate operates on a generic struct `Formula<B,U,A>` which can easily be used with your own `B`inary and `U`nary
operators and `Atom`ic formula types: if you can implement those types for your own preferred logic (modal, temporal, 
predicate, etc...) you can use the full functionality of this crate for your own language. A lot more information is in
the [docs](https://docs.rs/implies/0.2.1-alpha/implies) for this crate.

There are Python bindings for propositional logic, but using the API in Python gives much less control and flexibility.
You can use the Python APIs from Rust if you want by enabling the "python" feature when compiling, which will add "pyo3" as
a dependency.
