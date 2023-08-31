# implies: a Pybound Rust crate for logical formulas

**implies** is a Rust crate for storing logical formulas as trees and performing some basic operations on them,
like substitution, rotation, conversion to conjunctive normal form, and more. The crate currently implements the basic
functionality for propositional logic, with aims to extend for first-order (predicate) and modal logic. The basic structs
and traits should be easily personally extensible for any user of the crate who wants to use implies for their own logic. 

There are Python bindings for many of the methods, but using the API in Python gives much less control and flexibility.

