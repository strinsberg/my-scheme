use std::fmt;

// Builtin Procedures /////////////////////////////////////////////////////////

// NOTE when adding new procedures to the enum you have to add them to all three
// of the objects in this file. When adding them to the ALL_BUILTINS slice you
// need to update the size of the slice to match the number of elements.

// A way to properly identify builtin procedures so that their rust functions
// can be used and they appear different from Closures. The u8 is the arity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Cons,
    Car,
    Cdr,
    Eval,
    Apply,
    // arithmetic
    Sum,
    Subtract,
    Product,
    Divide,
    // predicates
    IsBool,
    IsSymbol,
    IsChar,
    IsNumber,
    IsString,
    IsProcedure,
    IsPair,
    IsVector,
    // Lists,
    SetCar,
    SetCdr,
    IsList,
    // Vectors
    MakeVec,
    Vector,
    VecSet,
    VecRef,
    VecLen,
    VecToList,
    VecFill,
    // TODO no push, pop, and concat/append in the standards???
    // other
    EQ,
    Eqv,
    BaseEnv,
}

// NOTE when you add elements update the size
pub const ALL_BUILTINS: &'static [Builtin; 30] = &[
    Builtin::Cons,
    Builtin::Car,
    Builtin::Cdr,
    Builtin::Eval,
    Builtin::Apply,
    // arithmetic
    Builtin::Sum,
    Builtin::Subtract,
    Builtin::Product,
    Builtin::Divide,
    // predicates
    Builtin::IsBool,
    Builtin::IsSymbol,
    Builtin::IsChar,
    Builtin::IsNumber,
    Builtin::IsString,
    Builtin::IsProcedure,
    Builtin::IsPair,
    Builtin::IsVector,
    // Lists,
    Builtin::SetCar,
    Builtin::SetCdr,
    Builtin::IsList,
    // Vectors
    Builtin::MakeVec,
    Builtin::Vector,
    Builtin::VecSet,
    Builtin::VecRef,
    Builtin::VecLen,
    Builtin::VecToList,
    Builtin::VecFill,
    // TODO no push, pop, and concat/append in the standards???
    // other
    Builtin::EQ,
    Builtin::Eqv,
    Builtin::BaseEnv,
];

// NOTE any enum value that has the same name as the identifier does not need to
// be added. I.e. Builtin::Cons => cons. However, any complex name or name that
// includes special symbols like -*!? etc. will need it's own match arm.
impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Builtin::Sum => "+",
            Builtin::Subtract => "-",
            Builtin::Product => "*",
            Builtin::Divide => "/",
            //
            Builtin::IsBool => "boolean?",
            Builtin::IsSymbol => "symbol?",
            Builtin::IsChar => "char?",
            Builtin::IsNumber => "number?",
            Builtin::IsString => "string?",
            Builtin::IsProcedure => "procedure?",
            Builtin::IsPair => "pair?",
            Builtin::IsVector => "vector?",
            //
            Builtin::SetCar => "set-car!",
            Builtin::SetCdr => "set-cdr!",
            Builtin::IsList => "list?",
            // Vectors
            Builtin::MakeVec => "make-vector",
            Builtin::VecSet => "vector-set!",
            Builtin::VecRef => "vector-ref",
            Builtin::VecLen => "vector-length",
            Builtin::VecToList => "vector->list",
            Builtin::VecFill => "vector-fill!",
            //
            Builtin::EQ => "eq?",
            Builtin::Eqv => "eqv?",
            Builtin::BaseEnv => "null-environment",
            b => return write!(f, "{}", format!("{:?}", b).to_lowercase()),
        };
        write!(f, "{}", s)
    }
}