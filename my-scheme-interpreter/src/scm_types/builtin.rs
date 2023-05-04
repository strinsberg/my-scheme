// Builtin Procedures /////////////////////////////////////////////////////////

// A way to properly identify builtin procedures so that their rust functions
// can be used and they appear different from Closures. The u8 is the arity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Cons,
    Car,
    Cdr,
    Eval,
    Apply,
    Sum,
    Subtract,
    Product,
    Divide,
    EQ,
    Eqv,
    BaseEnv,
}
