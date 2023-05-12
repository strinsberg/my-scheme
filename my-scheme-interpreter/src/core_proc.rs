use crate::builtin::Builtin;
use crate::error::{ScmErr, ValResult};
use crate::number::ScmNumber;
use crate::string::ScmString;
use crate::types::{Env, ScmVal};
use std::rc::Rc;

// All builtin functions that are not syntactic keywords and are the basic building
// blocks for all other functions. Syntactic keywords and things that require tail
// call optimization or also return an environment will be in eval_tco as they are
// special.
//
// TODO now that there are error routines for scheme that mirror what we have
// in rust write whatever seems appropriate from here in scheme.

// Apply Builtin //////////////////////////////////////////////////////////////

pub fn apply_core_proc(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    match op {
        Builtin::Cons => cons(args),
        Builtin::Car => car(args),
        Builtin::Cdr => cdr(args),
        Builtin::Eqv => eqv_proc(args),
        Builtin::BaseEnv => Ok(Env::new_null()),
        Builtin::Sum | Builtin::Subtract | Builtin::Product | Builtin::Divide => {
            arithmetic_proc(op, args)
        }
        // Predicates
        Builtin::IsBool => is_boolean(args),
        Builtin::IsChar => is_character(args),
        Builtin::IsSymbol => is_symbol(args),
        Builtin::IsNumber => is_number(args),
        Builtin::IsString => is_string(args),
        Builtin::IsPair => is_pair(args),
        Builtin::IsVector => is_vector(args),
        Builtin::IsProcedure => is_procedure(args),
        // lists
        Builtin::SetCar => set_car(args),
        Builtin::SetCdr => set_cdr(args),
        Builtin::IsList => is_list(args),
        Builtin::Length => list_length(args),
        Builtin::Reverse => list_reverse(args),
        Builtin::Append => list_append(args),
        // numbers
        Builtin::NumEq => num_eq(args),
        Builtin::NumLt => num_lt(args),
        Builtin::NumGt => num_gt(args),
        Builtin::NumLeq => num_leq(args),
        Builtin::NumGeq => num_geq(args),
        // Symbols
        Builtin::SymToStr => symbol_to_string(args),
        Builtin::StrToSym => string_to_symbol(args),
        // Chars
        Builtin::CharToInt => char_to_int(args),
        Builtin::IntToChar => int_to_char(args),
        Builtin::IsAlpha => is_alphabetic(args),
        Builtin::IsNumChar => is_numeric(args),
        Builtin::IsAplhaNum => is_alphanumeric(args),
        Builtin::IsWhite => is_whitespace(args),
        Builtin::IsUnsup => is_unsup(args),
        Builtin::IsUpper => is_uppercase(args),
        Builtin::IsLower => is_lowercase(args),
        Builtin::ToUpper => to_uppercase(args),
        Builtin::ToLower => to_lowercase(args),
        // Strings
        Builtin::MakeStr => make_string(args),
        Builtin::StrSet => string_set(args),
        Builtin::StrLen => string_length(args),
        Builtin::StrRef => string_ref(args),
        // Vectors
        Builtin::MakeVec => make_vector(args),
        Builtin::Vector => vector(args),
        Builtin::VecSet => vector_set(args),
        Builtin::VecRef => vector_ref(args),
        Builtin::VecLen => vector_length(args),
        // Error
        Builtin::Error => user_error(args),
        Builtin::ArgTypeError => user_arg_type_error(args),
        Builtin::RangeError => user_range_error(args),
        _ => panic!("unsupported builtin: {:?}", op),
    }
}

// Helpers ////////////////////////////////////////////////////////////////////

pub fn is_core_proc(val: ScmVal) -> bool {
    match val {
        ScmVal::Core(b, _) => match b {
            Builtin::Apply | Builtin::Eval => false,
            _ => true,
        },
        _ => false,
    }
}

pub fn is_closure(val: ScmVal) -> bool {
    match val {
        ScmVal::Closure(_) => true,
        _ => false,
    }
}

pub fn is_true(val: ScmVal) -> bool {
    match val {
        ScmVal::Boolean(false) => false,
        _ => true,
    }
}

// Core list functions ////////////////////////////////////////////////////////

pub fn cons(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => Ok(ScmVal::cons(args[0].clone(), args[1].clone())),
        _ => Err(ScmErr::Arity("cons".to_owned(), 2)),
    }
}

pub fn car(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Pair(cell) => Ok(cell.head.clone()),
            ScmVal::PairMut(cell) => Ok(cell.borrow().head.clone()),
            _ => Err(ScmErr::BadArgType(
                "car".to_owned(),
                "pair".to_owned(),
                args[0].clone(),
            )),
        }
    } else {
        Err(ScmErr::Arity("car".to_owned(), 1))
    }
}

pub fn cdr(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Pair(cell) => Ok(cell.tail.clone()),
            ScmVal::PairMut(cell) => Ok(cell.borrow().tail.clone()),
            _ => Err(ScmErr::BadArgType(
                "cdr".to_owned(),
                "pair".to_owned(),
                args[0].clone(),
            )),
        }
    } else {
        Err(ScmErr::Arity("cdr".to_owned(), 1))
    }
}

// Equivalence ////////////////////////////////////////////////////////////////

// TODO try to make this more robust and closer to the R5RS expectations.
// Now that we use pointers some stuff may be easier to do.
fn eqv_proc(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => Ok(ScmVal::Boolean(eqv(args[0].clone(), args[1].clone()))),
        _ => Err(ScmErr::Arity("eqv?".to_owned(), 2)),
    }
}

// not this simple, but will do for now
pub fn eqv(a: ScmVal, b: ScmVal) -> bool {
    a == b
}

// Core Arithmetic //////////////////////////////////////////////////////////

pub fn arithmetic_proc(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        1 => unary_arithmetic(op, args),
        2 => binary_arithmetic(op, args),
        3.. => sum_arithmetic(op, args),
        _ => Err(ScmErr::Arity(format! {"{}", op}, 1)),
    }
}

fn binary_arithmetic(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    let left = args[0].clone();
    let right = args[1].clone();
    let type_str = "number".to_string();

    let result = match (left.clone(), right.clone()) {
        (ScmVal::Number(l), ScmVal::Number(r)) => match op.clone() {
            Builtin::Sum => l.add(r.clone()),
            Builtin::Subtract => l.subtract(r.clone()),
            Builtin::Product => l.multiply(r.clone()),
            Builtin::Divide => l.divide(r.clone()),
            _ => panic!("operation should be arithmetic procedure: {:?}", op),
        },
        (ScmVal::Number(_), r) => return Err(ScmErr::BadArgType(format!("{}", op), type_str, r)),
        (l, _) => return Err(ScmErr::BadArgType(format!("{}", op), type_str, l)),
    };

    Ok(ScmVal::Number(result.ok_or(ScmErr::BadArithmetic(
        op.to_string(),
        left,
        right,
    ))?))
}

fn unary_arithmetic(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    match op {
        Builtin::Subtract => binary_arithmetic(op, vec![ScmVal::new_int(0), args[0].clone()]),
        Builtin::Divide => binary_arithmetic(op, vec![ScmVal::new_int(1), args[0].clone()]),
        Builtin::Product => Ok(args[0].clone()),
        Builtin::Sum => Ok(args[0].clone()),
        _ => {
            return Err(ScmErr::BadArgType(
                format!("{}", op),
                "number".to_owned(),
                args[0].clone(),
            ))
        }
    }
}

fn sum_arithmetic(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    let mut result = args[0].clone();
    for num in args[1..].iter() {
        result = binary_arithmetic(op.clone(), vec![result, num.clone()])?;
    }
    Ok(result)
}

pub fn is_arithmetic_builtin(val: Builtin) -> bool {
    match val {
        Builtin::Sum | Builtin::Subtract | Builtin::Product | Builtin::Divide => true,
        _ => false,
    }
}

// Core Predicates ////////////////////////////////////////////////////////////

pub fn is_boolean(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Boolean(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("boolean?".to_owned(), 1))
    }
}

pub fn is_character(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Character(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("char?".to_owned(), 1))
    }
}

pub fn is_symbol(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Symbol(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("symbol?".to_owned(), 1))
    }
}

pub fn is_number(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Number(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("number?".to_owned(), 1))
    }
}

pub fn is_string(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::String(_) | ScmVal::StringMut(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("string?".to_owned(), 1))
    }
}

pub fn is_pair(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Pair(_) | ScmVal::PairMut(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("pair?".to_owned(), 1))
    }
}

pub fn is_vector(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Vector(_) | ScmVal::VectorMut(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("vector?".to_owned(), 1))
    }
}

pub fn is_procedure(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Core(_, _) | ScmVal::Closure(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("procedure?".to_owned(), 1))
    }
}

// Lists //////////////////////////////////////////////////////////////////////

pub fn set_car(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 2 {
        match args[0].clone() {
            ScmVal::PairMut(cell) => cell.borrow_mut().head = args[1].clone(),
            _ => {
                return Err(ScmErr::BadArgType(
                    "set-car!".to_owned(),
                    "mutable pair".to_owned(),
                    args[0].clone(),
                ))
            }
        }
    } else {
        return Err(ScmErr::Arity("set-car!".to_owned(), 2));
    }
    Ok(ScmVal::Empty)
}

pub fn set_cdr(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 2 {
        match args[0].clone() {
            ScmVal::PairMut(cell) => cell.borrow_mut().tail = args[1].clone(),
            _ => {
                return Err(ScmErr::BadArgType(
                    "set-cdr!".to_owned(),
                    "mutable pair".to_owned(),
                    args[0].clone(),
                ))
            }
        }
    } else {
        return Err(ScmErr::Arity("set-cdr!".to_owned(), 2));
    }
    Ok(ScmVal::Empty)
}

pub fn is_list(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        let (_, dotted, cyclic) = match ScmVal::list_to_vec(args[0].clone()) {
            Some(res) => res,
            None => return Ok(ScmVal::Boolean(false)),
        };

        if dotted || cyclic {
            Ok(ScmVal::Boolean(false))
        } else {
            Ok(ScmVal::Boolean(true))
        }
    } else {
        Err(ScmErr::Arity("list?".to_owned(), 1))
    }
}

pub fn list_length(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        let (vec, dotted, cyclic) =
            ScmVal::list_to_vec(args[0].clone()).ok_or(ScmErr::BadArgType(
                "length".to_owned(),
                "proper list".to_owned(),
                args[0].clone(),
            ))?;
        if dotted || cyclic {
            Err(ScmErr::BadArgType(
                "length".to_owned(),
                "proper list".to_owned(),
                args[0].clone(),
            ))
        } else {
            Ok(ScmVal::new_int(vec.len() as i64))
        }
    } else {
        Err(ScmErr::Arity("length".to_owned(), 1))
    }
}

pub fn list_append(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        let shared = args[args.len() - 1].clone();
        let mut appended = Vec::new();

        for arg in args[..args.len() - 1].into_iter() {
            let (arg_vec, dotted, cyclic) =
                ScmVal::list_to_vec(arg.clone()).ok_or(ScmErr::BadArgType(
                    "append".to_owned(),
                    "proper list".to_owned(),
                    args[0].clone(),
                ))?;
            if dotted || cyclic {
                return Err(ScmErr::BadArgType(
                    "append".to_owned(),
                    "proper list".to_owned(),
                    args[0].clone(),
                ));
            } else {
                for a in arg_vec.into_iter() {
                    appended.push(a)
                }
            }
        }
        Ok(ScmVal::vec_to_list(appended, shared))
    } else {
        Err(ScmErr::Arity("append".to_owned(), 1))
    }
}

pub fn list_reverse(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        let (arg_vec, dotted, cyclic) =
            ScmVal::list_to_vec(args[0].clone()).ok_or(ScmErr::BadArgType(
                "reverse".to_owned(),
                "proper list".to_owned(),
                args[0].clone(),
            ))?;
        if dotted || cyclic {
            return Err(ScmErr::BadArgType(
                "reverse".to_owned(),
                "proper list".to_owned(),
                args[0].clone(),
            ));
        } else {
            Ok(ScmVal::vec_to_list(
                arg_vec.into_iter().rev().collect(),
                ScmVal::Empty,
            ))
        }
    } else {
        Err(ScmErr::Arity("reverse".to_owned(), 1))
    }
}

// Symbols ////////////////////////////////////////////////////////////////////

pub fn symbol_to_string(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("symbol->string".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Symbol(s) => Ok(ScmVal::String(s)),
        _ => Err(ScmErr::BadArgType(
            "symbol->string".to_owned(),
            "symbol".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn string_to_symbol(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("string->symbol".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::String(s) => Ok(ScmVal::Symbol(s)),
        ScmVal::StringMut(s) => Ok(ScmVal::Symbol(Rc::new((*s.borrow()).clone()))),
        _ => Err(ScmErr::BadArgType(
            "symbol->string".to_owned(),
            "symbol".to_owned(),
            args[0].clone(),
        )),
    }
}

// Chars //////////////////////////////////////////////////////////////////////

pub fn char_to_int(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char->integer".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::new_int(ch.to_int())),
        _ => Err(ScmErr::BadArgType(
            "char->integer".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn int_to_char(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("integer->char".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Number(ScmNumber::Integer(i)) => Ok(ScmVal::new_char((i as u8) as char)),
        _ => Err(ScmErr::BadArgType(
            "integer->char".to_owned(),
            "exact non-negative integer".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_alphabetic(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-alphabetic?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_alpha())),
        _ => Err(ScmErr::BadArgType(
            "char-alphabetic?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_numeric(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-numeric?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_numeric())),
        _ => Err(ScmErr::BadArgType(
            "char-numeric?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_alphanumeric(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-alphanumeric?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_alphanumeric())),
        _ => Err(ScmErr::BadArgType(
            "char-alphanumeric?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_whitespace(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-whitespace?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_whitespace())),
        _ => Err(ScmErr::BadArgType(
            "char-whitespace?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_unsup(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-unsup?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_unsup())),
        _ => Err(ScmErr::BadArgType(
            "char-unsup?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_uppercase(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-upper-case?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_upper_case())),
        _ => Err(ScmErr::BadArgType(
            "char-upper-case?".to_owned(),
            "letter".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_lowercase(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-lower-case?".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_lower_case())),
        _ => Err(ScmErr::BadArgType(
            "char-lower-case?".to_owned(),
            "letter".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn to_uppercase(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-upcase".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Character(ch.to_upper_case())),
        _ => Err(ScmErr::BadArgType(
            "char-upcase".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn to_lowercase(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("char-downcase".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Character(ch.to_lower_case())),
        _ => Err(ScmErr::BadArgType(
            "char-downcase".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

// Numbers ////////////////////////////////////////////////////////////////////

pub fn num_eq(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("=".to_owned(), 2));
    }
    match (args[0].clone(), args[1].clone()) {
        (ScmVal::Number(num1), ScmVal::Number(num2)) => Ok(ScmVal::Boolean(num1 == num2)),
        (ScmVal::Number(_), _) => Err(ScmErr::BadArgType(
            "=".to_owned(),
            "number".to_owned(),
            args[1].clone(),
        )),
        _ => Err(ScmErr::BadArgType(
            "=".to_owned(),
            "number".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn num_lt(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("<".to_owned(), 2));
    }
    match (args[0].clone(), args[1].clone()) {
        (ScmVal::Number(num1), ScmVal::Number(num2)) => Ok(ScmVal::Boolean(num1 < num2)),
        (ScmVal::Number(_), _) => Err(ScmErr::BadArgType(
            "<".to_owned(),
            "number".to_owned(),
            args[1].clone(),
        )),
        _ => Err(ScmErr::BadArgType(
            "<".to_owned(),
            "number".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn num_gt(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity(">".to_owned(), 2));
    }
    match (args[0].clone(), args[1].clone()) {
        (ScmVal::Number(num1), ScmVal::Number(num2)) => Ok(ScmVal::Boolean(num1 > num2)),
        (ScmVal::Number(_), _) => Err(ScmErr::BadArgType(
            ">".to_owned(),
            "number".to_owned(),
            args[1].clone(),
        )),
        _ => Err(ScmErr::BadArgType(
            ">".to_owned(),
            "number".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn num_leq(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("<=".to_owned(), 2));
    }
    match (args[0].clone(), args[1].clone()) {
        (ScmVal::Number(num1), ScmVal::Number(num2)) => Ok(ScmVal::Boolean(num1 <= num2)),
        (ScmVal::Number(_), _) => Err(ScmErr::BadArgType(
            "<=".to_owned(),
            "number".to_owned(),
            args[1].clone(),
        )),
        _ => Err(ScmErr::BadArgType(
            "<=".to_owned(),
            "number".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn num_geq(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity(">=".to_owned(), 2));
    }
    match (args[0].clone(), args[1].clone()) {
        (ScmVal::Number(num1), ScmVal::Number(num2)) => Ok(ScmVal::Boolean(num1 >= num2)),
        (ScmVal::Number(_), _) => Err(ScmErr::BadArgType(
            ">=".to_owned(),
            "number".to_owned(),
            args[1].clone(),
        )),
        _ => Err(ScmErr::BadArgType(
            ">=".to_owned(),
            "number".to_owned(),
            args[0].clone(),
        )),
    }
}

// Strings ////////////////////////////////////////////////////////////////////

pub fn make_string(args: Vec<ScmVal>) -> ValResult {
    let size = match args.len() {
        1 | 2 => match args[0].clone() {
            ScmVal::Number(ScmNumber::Integer(i)) => i,
            _ => {
                return Err(ScmErr::BadArgType(
                    "make-string".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[0].clone(),
                ))
            }
        },
        _ => return Err(ScmErr::Arity("make-string".to_owned(), 1)),
    };

    if size < 0 {
        return Err(ScmErr::BadArgType(
            "make-string".to_owned(),
            "exact non-negative integer".to_owned(),
            args[0].clone(),
        ));
    }

    let fill: u8 = match args.len() {
        2 => match args[1].clone() {
            ScmVal::Character(ch) => ch.to_byte(),
            _ => {
                return Err(ScmErr::BadArgType(
                    "make-string".to_owned(),
                    "char".to_owned(),
                    args[1].clone(),
                ))
            }
        },
        _ => 0,
    };

    Ok(ScmVal::new_str_mut_from_scmstring(ScmString::from_bytes(
        &vec![fill; size as usize],
    )))
}

pub fn string_set(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("string-set!".to_owned(), 3));
    }

    let index = match args[1].clone() {
        ScmVal::Number(ScmNumber::Integer(i)) => match i {
            0.. => i as usize,
            _ => {
                return Err(ScmErr::BadArgType(
                    "string-set!".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[1].clone(),
                ))
            }
        },
        _ => {
            return Err(ScmErr::BadArgType(
                "string-set!".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

    match args[0].clone() {
        ScmVal::StringMut(s) => {
            if s.borrow().chars.len() > index {
                s.borrow_mut().chars[index] = match args[2].clone() {
                    ScmVal::Character(ch) => ch.clone(),
                    _ => {
                        return Err(ScmErr::BadArgType(
                            "string-set!".to_owned(),
                            "mutable string".to_owned(),
                            args[2].clone(),
                        ))
                    }
                }
            } else {
                return Err(ScmErr::RangeError(
                    "string-set!".to_owned(),
                    args[1].clone(),
                    args[0].clone(),
                ));
            }
        }
        _ => {
            return Err(ScmErr::BadArgType(
                "string-set!".to_owned(),
                "mutable string".to_owned(),
                args[0].clone(),
            ))
        }
    };

    Ok(ScmVal::Empty)
}

pub fn string_length(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("string-length".to_owned(), 1));
    }

    match args[0].clone() {
        ScmVal::StringMut(s) => Ok(ScmVal::new_int(s.borrow().chars.len() as i64)),
        ScmVal::String(s) => Ok(ScmVal::new_int(s.chars.len() as i64)),
        _ => Err(ScmErr::BadArgType(
            "string-length".to_owned(),
            "string".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn string_ref(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("string-ref".to_owned(), 2));
    }

    let index = match args[1].clone() {
        ScmVal::Number(ScmNumber::Integer(i)) => match i {
            0.. => i as usize,
            _ => {
                return Err(ScmErr::BadArgType(
                    "string-ref".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[1].clone(),
                ))
            }
        },
        _ => {
            return Err(ScmErr::BadArgType(
                "string-ref".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

    match args[0].clone() {
        ScmVal::StringMut(s) => {
            if s.borrow().chars.len() > index {
                Ok(ScmVal::Character(s.borrow().chars[index].clone()))
            } else {
                Err(ScmErr::RangeError(
                    "string-ref".to_owned(),
                    args[1].clone(),
                    args[0].clone(),
                ))
            }
        }
        ScmVal::String(s) => {
            if s.chars.len() > index {
                Ok(ScmVal::Character(s.chars[index].clone()))
            } else {
                Err(ScmErr::RangeError(
                    "string-ref".to_owned(),
                    args[1].clone(),
                    args[0].clone(),
                ))
            }
        }
        _ => Err(ScmErr::BadArgType(
            "string-ref".to_owned(),
            "string".to_owned(),
            args[0].clone(),
        )),
    }
}

// Vector /////////////////////////////////////////////////////////////////////

pub fn vector(args: Vec<ScmVal>) -> ValResult {
    Ok(ScmVal::new_vec_mut(args))
}

pub fn vector_length(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("vector-length".to_owned(), 2));
    }

    match args[0].clone() {
        ScmVal::VectorMut(vec) => Ok(ScmVal::new_int(vec.borrow().len() as i64)),
        ScmVal::Vector(vec) => Ok(ScmVal::new_int(vec.len() as i64)),
        _ => Err(ScmErr::BadArgType(
            "vector-length".to_owned(),
            "vector".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn make_vector(args: Vec<ScmVal>) -> ValResult {
    let size = match args.len() {
        1 | 2 => match args[0].clone() {
            ScmVal::Number(ScmNumber::Integer(i)) => i,
            _ => {
                return Err(ScmErr::BadArgType(
                    "make-vector".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[0].clone(),
                ))
            }
        },
        _ => return Err(ScmErr::Arity("make-vector".to_owned(), 1)),
    };

    if size < 0 {
        return Err(ScmErr::BadArgType(
            "make-vector".to_owned(),
            "exact non-negative integer".to_owned(),
            args[0].clone(),
        ));
    }

    let fill = match args.len() {
        2 => args[1].clone(),
        _ => ScmVal::new_int(0),
    };

    Ok(ScmVal::new_vec_mut(vec![fill; size as usize]))
}

pub fn vector_set(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("vector-set!".to_owned(), 3));
    }

    let index = match args[1].clone() {
        ScmVal::Number(ScmNumber::Integer(i)) => match i {
            0.. => i as usize,
            _ => {
                return Err(ScmErr::BadArgType(
                    "vector-set!".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[1].clone(),
                ))
            }
        },
        _ => {
            return Err(ScmErr::BadArgType(
                "vector-set!".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

    match args[0].clone() {
        ScmVal::VectorMut(vec) => {
            if vec.borrow().len() > index {
                vec.borrow_mut()[index] = args[2].clone();
            } else {
                return Err(ScmErr::OutOfBounds(index, vec.borrow().len()));
            }
        }
        _ => {
            return Err(ScmErr::BadArgType(
                "vector-set!".to_owned(),
                "mutable vector".to_owned(),
                args[0].clone(),
            ))
        }
    };

    Ok(ScmVal::Empty)
}

pub fn vector_ref(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("vector-ref".to_owned(), 2));
    }

    let index = match args[1].clone() {
        ScmVal::Number(ScmNumber::Integer(i)) => match i {
            0.. => i as usize,
            _ => {
                return Err(ScmErr::BadArgType(
                    "vector-ref".to_owned(),
                    "exact non-negative integer".to_owned(),
                    args[1].clone(),
                ))
            }
        },
        _ => {
            return Err(ScmErr::BadArgType(
                "vector-ref".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

    match args[0].clone() {
        ScmVal::VectorMut(vec) => {
            if vec.borrow().len() > index {
                Ok(vec.borrow()[index].clone())
            } else {
                Err(ScmErr::OutOfBounds(index, vec.borrow().len()))
            }
        }
        ScmVal::Vector(vec) => {
            if vec.len() > index {
                Ok(vec[index].clone())
            } else {
                Err(ScmErr::OutOfBounds(index, vec.len()))
            }
        }
        _ => Err(ScmErr::BadArgType(
            "vector-ref".to_owned(),
            "vector".to_owned(),
            args[0].clone(),
        )),
    }
}

// Errors /////////////////////////////////////////////////////////////////////

fn user_error(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("error!".to_owned(), 2));
    }

    Err(ScmErr::UserError(
        args[0].to_string(),
        args[1].to_string(),
        args[2..].into(),
    ))
}

fn user_arg_type_error(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("arg-type-error!".to_owned(), 3));
    }

    Err(ScmErr::BadArgType(
        args[0].to_string(),
        args[2].to_string(),
        args[1].clone(),
    ))
}

fn user_range_error(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("range-error!".to_owned(), 3));
    }

    Err(ScmErr::RangeError(
        args[0].to_string(),
        args[1].clone(),
        args[2].clone(),
    ))
}
