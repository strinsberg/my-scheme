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
    Length,
    Reverse,
    Append,
    // Numbers
    NumEq,
    NumLt,
    NumGt,
    NumLeq,
    NumGeq,
    // Symbols
    SymToStr,
    StrToSym,
    // Characters
    CharToInt,
    IntToChar,
    IsAlpha,
    IsAplhaNum,
    IsNumChar,
    IsWhite,
    IsUnsup,
    IsUpper,
    IsLower,
    ToUpper,
    ToLower,
    // Strings
    MakeStr,
    StrSet,
    StrLen,
    StrRef,
    // Vectors
    MakeVec,
    Vector,
    VecSet,
    VecRef,
    VecLen,
    // TODO no push, pop, and concat/append in the standards???
    // other
    EQ,
    Eqv,
    BaseEnv,
    // Errors
    Error,
    ArgTypeError,
    RangeError,
}

// NOTE when you add elements update the size
pub const ALL_BUILTINS: &'static [(Builtin, u8); 56] = &[
    (Builtin::Cons, 2),
    (Builtin::Car, 1),
    (Builtin::Cdr, 1),
    (Builtin::Eval, 2),
    (Builtin::Apply, 2),
    // arithmetic
    (Builtin::Sum, 1),
    (Builtin::Subtract, 1),
    (Builtin::Product, 1),
    (Builtin::Divide, 1),
    // predicates
    (Builtin::IsBool, 1),
    (Builtin::IsSymbol, 1),
    (Builtin::IsChar, 1),
    (Builtin::IsNumber, 1),
    (Builtin::IsString, 1),
    (Builtin::IsProcedure, 1),
    (Builtin::IsPair, 1),
    (Builtin::IsVector, 1),
    // Lists
    (Builtin::SetCar, 2),
    (Builtin::SetCdr, 2),
    (Builtin::IsList, 1),
    (Builtin::Length, 1),
    (Builtin::Reverse, 1),
    (Builtin::Append, 2),
    // Numbers
    (Builtin::NumEq, 2),
    (Builtin::NumLt, 2),
    (Builtin::NumGt, 2),
    (Builtin::NumLeq, 2),
    (Builtin::NumGeq, 2),
    // Symbols
    (Builtin::SymToStr, 1),
    (Builtin::StrToSym, 1),
    // Chars
    (Builtin::CharToInt, 1),
    (Builtin::IntToChar, 1),
    (Builtin::IsAlpha, 1),
    (Builtin::IsAplhaNum, 1),
    (Builtin::IsNumChar, 1),
    (Builtin::IsWhite, 1),
    (Builtin::IsUnsup, 1),
    (Builtin::IsUpper, 1),
    (Builtin::IsLower, 1),
    (Builtin::ToUpper, 1),
    (Builtin::ToLower, 1),
    // Strings
    (Builtin::MakeStr, 1),
    (Builtin::StrSet, 3),
    (Builtin::StrLen, 1),
    (Builtin::StrRef, 2),
    // Vectors
    (Builtin::MakeVec, 1),
    (Builtin::Vector, 0),
    (Builtin::VecSet, 3),
    (Builtin::VecRef, 2),
    (Builtin::VecLen, 1),
    // other
    (Builtin::EQ, 2),
    (Builtin::Eqv, 2),
    (Builtin::BaseEnv, 0),
    // errors
    (Builtin::Error, 2),
    (Builtin::ArgTypeError, 3),
    (Builtin::RangeError, 3),
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
            //
            Builtin::NumEq => "=",
            Builtin::NumLt => "<",
            Builtin::NumGt => ">",
            Builtin::NumLeq => "<=",
            Builtin::NumGeq => ">=",
            //
            Builtin::SymToStr => "symbol->string",
            Builtin::StrToSym => "string->symbol",
            //
            Builtin::CharToInt => "char->integer",
            Builtin::IntToChar => "integer->char",
            Builtin::IsAlpha => "char-alphabetic?",
            Builtin::IsAplhaNum => "char-alphanumeric?",
            Builtin::IsNumChar => "char-numeric?",
            Builtin::IsWhite => "char-whitespace?",
            Builtin::IsUnsup => "char-unsup?",
            Builtin::IsUpper => "char-upper-case?",
            Builtin::IsLower => "char-lower-case?",
            Builtin::ToUpper => "char-upcase",
            Builtin::ToLower => "char-downcase",
            //
            Builtin::MakeStr => "make-string",
            Builtin::StrSet => "string-set!",
            Builtin::StrLen => "string-length",
            Builtin::StrRef => "string-ref",
            //
            Builtin::MakeVec => "make-vector",
            Builtin::VecSet => "vector-set!",
            Builtin::VecRef => "vector-ref",
            Builtin::VecLen => "vector-length",
            //
            Builtin::EQ => "eq?",
            Builtin::Eqv => "eqv?",
            Builtin::BaseEnv => "null-environment",
            //
            Builtin::Error => "error!",
            Builtin::ArgTypeError => "arg-type-error!",
            Builtin::RangeError => "range-error!",
            //
            b => return write!(f, "{}", format!("{:?}", b).to_lowercase()),
        };
        write!(f, "{}", s)
    }
}
