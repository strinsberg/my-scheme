use crate::builtin::Builtin;
use crate::error::{ScmErr, ValResult};
use crate::number::Num;
use crate::string::ScmString;
use crate::types::{Env, ScmVal};

// All builtin functions that are not syntactic keywords and are the basic building
// blocks for all other functions. Syntactic keywords and things that require tail
// call optimization or also return an environment will be in eval_tco as they are
// special.

// Apply Builtin //////////////////////////////////////////////////////////////

pub fn apply_core_proc(op: Builtin, args: &[ScmVal]) -> ValResult {
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

pub fn cons(args: &[ScmVal]) -> ValResult {
    Ok(ScmVal::cons(args[0].clone(), args[1].clone()))
}

pub fn car(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewPair(cell) => Ok(cell.clone_head()),
        _ => Err(ScmErr::BadArgType(
            "car".to_owned(),
            "pair".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn cdr(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewPair(cell) => Ok(cell.clone_tail()),
        _ => Err(ScmErr::BadArgType(
            "cdr".to_owned(),
            "pair".to_owned(),
            args[0].clone(),
        )),
    }
}

// Equivalence ////////////////////////////////////////////////////////////////

// TODO try to make this more robust and closer to the R5RS expectations.
// Now that we use pointers some stuff may be easier to do.
fn eqv_proc(args: &[ScmVal]) -> ValResult {
    Ok(ScmVal::Boolean(eqv(args[0].clone(), args[1].clone())))
}

// not this simple, but will do for now
pub fn eqv(a: ScmVal, b: ScmVal) -> bool {
    a == b
}

// Core Arithmetic //////////////////////////////////////////////////////////

pub fn arithmetic_proc(op: Builtin, args: &[ScmVal]) -> ValResult {
    match args.len() {
        1 => unary_arithmetic(op, args),
        2 => binary_arithmetic(op, args),
        3.. => sum_arithmetic(op, args),
        _ => Err(ScmErr::Arity(format! {"{}", op}, 1)),
    }
}

fn binary_arithmetic(op: Builtin, args: &[ScmVal]) -> ValResult {
    let left = args[0].clone();
    let right = args[1].clone();
    let type_str = "number".to_string();

    let result = match (left.clone(), right.clone()) {
        (ScmVal::Number(l), ScmVal::Number(r)) => match op.clone() {
            Builtin::Sum => l.add(&r),
            Builtin::Subtract => l.sub(&r),
            Builtin::Product => l.mult(&r),
            Builtin::Divide => l.div(&r),
            _ => panic!("operation should be arithmetic procedure: {:?}", op),
        },
        (ScmVal::Number(_), r) => return Err(ScmErr::BadArgType(format!("{}", op), type_str, r)),
        (l, _) => return Err(ScmErr::BadArgType(format!("{}", op), type_str, l)),
    };

    match result {
        Ok(val) => Ok(ScmVal::Number(val)),
        Err(_) => Err(ScmErr::BadArithmetic(op.to_string(), left, right)),
    }
}

fn unary_arithmetic(op: Builtin, args: &[ScmVal]) -> ValResult {
    match op {
        Builtin::Subtract => binary_arithmetic(op, &[ScmVal::new_int(0), args[0].clone()]),
        Builtin::Divide => binary_arithmetic(op, &[ScmVal::new_int(1), args[0].clone()]),
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

fn sum_arithmetic(op: Builtin, args: &[ScmVal]) -> ValResult {
    let mut result = args[0].clone();
    for num in args[1..].iter() {
        result = binary_arithmetic(op.clone(), &[result, num.clone()])?;
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

pub fn is_boolean(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Boolean(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_character(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_symbol(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewSymbol(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_number(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Number(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_string(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewString(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_pair(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewPair(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_vector(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewVec(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

pub fn is_procedure(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Core(_, _) | ScmVal::Closure(_) => Ok(ScmVal::Boolean(true)),
        _ => Ok(ScmVal::Boolean(false)),
    }
}

// Lists //////////////////////////////////////////////////////////////////////

pub fn set_car(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewPair(cell) if cell.mutable => {
            cell.set_head(args[1].clone())
                .expect("cell should be mutable");
        }
        _ => {
            return Err(ScmErr::BadArgType(
                "set-car!".to_owned(),
                "mutable pair".to_owned(),
                args[0].clone(),
            ))
        }
    }
    Ok(ScmVal::Empty)
}

pub fn set_cdr(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewPair(cell) if cell.mutable => {
            cell.set_tail(args[1].clone())
                .expect("cell should be mutable");
        }
        _ => {
            return Err(ScmErr::BadArgType(
                "set-cdr!".to_owned(),
                "mutable pair".to_owned(),
                args[0].clone(),
            ))
        }
    }
    Ok(ScmVal::Empty)
}

// TODO no longer checks for cyclic
pub fn is_list(args: &[ScmVal]) -> ValResult {
    let (_, dotted) = match ScmVal::list_to_vec(&args[0]) {
        Some(res) => res,
        None => return Ok(ScmVal::Boolean(false)),
    };

    if dotted {
        Ok(ScmVal::Boolean(false))
    } else {
        Ok(ScmVal::Boolean(true))
    }
}

// TODO no longer checks for cyclic
// TODO can we combine a helper to do both is_list check and return length
pub fn list_length(args: &[ScmVal]) -> ValResult {
    let (vec, dotted) = ScmVal::list_to_vec(&args[0]).ok_or(ScmErr::BadArgType(
        "length".to_owned(),
        "proper list".to_owned(),
        args[0].clone(),
    ))?;
    if dotted {
        Err(ScmErr::BadArgType(
            "length".to_owned(),
            "proper list".to_owned(),
            args[0].clone(),
        ))
    } else {
        Ok(ScmVal::new_int(vec.len() as i64))
    }
}

// TODO no longer checks cyclic
pub fn list_append(args: &[ScmVal]) -> ValResult {
    let shared = args[args.len() - 1].clone();
    let mut appended = Vec::new();

    for arg in args[..args.len() - 1].into_iter() {
        let (arg_vec, dotted) = ScmVal::list_to_vec(&arg).ok_or(ScmErr::BadArgType(
            "append".to_owned(),
            "proper list".to_owned(),
            args[0].clone(),
        ))?;
        if dotted {
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
    Ok(ScmVal::vec_to_list(&appended, shared))
}

// TODO no longer checks cyclic
pub fn list_reverse(args: &[ScmVal]) -> ValResult {
    let (arg_vec, dotted) = ScmVal::list_to_vec(&args[0]).ok_or(ScmErr::BadArgType(
        "reverse".to_owned(),
        "proper list".to_owned(),
        args[0].clone(),
    ))?;
    if dotted {
        return Err(ScmErr::BadArgType(
            "reverse".to_owned(),
            "proper list".to_owned(),
            args[0].clone(),
        ));
    } else {
        let reversed_vec: Vec<ScmVal> = arg_vec.into_iter().rev().collect();
        Ok(ScmVal::vec_to_list(&reversed_vec, ScmVal::Empty))
    }
}

// Symbols ////////////////////////////////////////////////////////////////////

pub fn symbol_to_string(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewSymbol(s) => Ok(ScmVal::NewString(s)),
        _ => Err(ScmErr::BadArgType(
            "symbol->string".to_owned(),
            "symbol".to_owned(),
            args[0].clone(),
        )),
    }
}
pub fn string_to_symbol(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewString(s) if s.mutable => Ok(ScmVal::NewSymbol(s)),
        ScmVal::NewString(s) if !s.mutable => Ok(ScmVal::new_sym(&s.to_string())),
        _ => Err(ScmErr::BadArgType(
            "symbol->string".to_owned(),
            "symbol".to_owned(),
            args[0].clone(),
        )),
    }
}

// Chars //////////////////////////////////////////////////////////////////////

pub fn char_to_int(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::new_int(ch.to_int())),
        _ => Err(ScmErr::BadArgType(
            "char->integer".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn int_to_char(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Number(Num::Int(i)) => Ok(ScmVal::new_char((i as u8) as char)),
        _ => Err(ScmErr::BadArgType(
            "integer->char".to_owned(),
            "exact non-negative integer".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_alphabetic(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_alpha())),
        _ => Err(ScmErr::BadArgType(
            "char-alphabetic?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_numeric(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_numeric())),
        _ => Err(ScmErr::BadArgType(
            "char-numeric?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_alphanumeric(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_alphanumeric())),
        _ => Err(ScmErr::BadArgType(
            "char-alphanumeric?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_whitespace(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_whitespace())),
        _ => Err(ScmErr::BadArgType(
            "char-whitespace?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_unsup(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_unsup())),
        _ => Err(ScmErr::BadArgType(
            "char-unsup?".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_uppercase(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_upper_case())),
        _ => Err(ScmErr::BadArgType(
            "char-upper-case?".to_owned(),
            "letter".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn is_lowercase(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Boolean(ch.is_lower_case())),
        _ => Err(ScmErr::BadArgType(
            "char-lower-case?".to_owned(),
            "letter".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn to_uppercase(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::Character(ch) => Ok(ScmVal::Character(ch.to_upper_case())),
        _ => Err(ScmErr::BadArgType(
            "char-upcase".to_owned(),
            "char".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn to_lowercase(args: &[ScmVal]) -> ValResult {
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

pub fn num_eq(args: &[ScmVal]) -> ValResult {
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

pub fn num_lt(args: &[ScmVal]) -> ValResult {
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

pub fn num_gt(args: &[ScmVal]) -> ValResult {
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

pub fn num_leq(args: &[ScmVal]) -> ValResult {
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
pub fn num_geq(args: &[ScmVal]) -> ValResult {
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

pub fn make_string(args: &[ScmVal]) -> ValResult {
    let size = match args[0].clone() {
        ScmVal::Number(Num::Int(i)) if i >= 0 => i,
        _ => {
            return Err(ScmErr::BadArgType(
                "make-string".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

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
        _ => '\0' as u8,
    };

    Ok(ScmVal::from_scm_str(
        ScmString::from_bytes(&vec![fill; size as usize]),
        true,
    ))
}

pub fn string_set(args: &[ScmVal]) -> ValResult {
    let index = to_index(&args[1]).ok_or(ScmErr::BadArgType(
        "string-set!".to_owned(),
        "exact non-negative integer".to_owned(),
        args[1].clone(),
    ))?;

    match args[0].clone() {
        ScmVal::NewString(s) if s.mutable => {
            let ch = match args[2].clone() {
                ScmVal::Character(ch) => ch.clone(),
                _ => {
                    return Err(ScmErr::BadArgType(
                        "string-set!".to_owned(),
                        "char".to_owned(),
                        args[2].clone(),
                    ))
                }
            };
            if !s.set_char(ch, index) {
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

pub fn string_length(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewString(s) => Ok(ScmVal::new_int(s.len() as i64)),
        _ => Err(ScmErr::BadArgType(
            "string-length".to_owned(),
            "string".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn string_ref(args: &[ScmVal]) -> ValResult {
    let index = to_index(&args[1]).ok_or(ScmErr::BadArgType(
        "string-ref".to_owned(),
        "exact non-negative integer".to_owned(),
        args[1].clone(),
    ))?;

    match args[0].clone() {
        ScmVal::NewString(s) => Ok(ScmVal::Character(s.get_char(index).ok_or(
            ScmErr::RangeError("string-ref".to_owned(), args[1].clone(), args[0].clone()),
        )?)),
        _ => Err(ScmErr::BadArgType(
            "string-ref".to_owned(),
            "string".to_owned(),
            args[0].clone(),
        )),
    }
}

// Vector /////////////////////////////////////////////////////////////////////

pub fn vector(args: &[ScmVal]) -> ValResult {
    Ok(ScmVal::new_vec_mut(args.to_vec()))
}

pub fn vector_length(args: &[ScmVal]) -> ValResult {
    match args[0].clone() {
        ScmVal::NewVec(vec) => Ok(ScmVal::new_int(vec.len() as i64)),
        _ => Err(ScmErr::BadArgType(
            "vector-length".to_owned(),
            "vector".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn make_vector(args: &[ScmVal]) -> ValResult {
    let size = match args[0].clone() {
        ScmVal::Number(Num::Int(i)) if i >= 0 => i,
        _ => {
            return Err(ScmErr::BadArgType(
                "make-vector".to_owned(),
                "exact non-negative integer".to_owned(),
                args[0].clone(),
            ))
        }
    };

    let fill = match args.len() {
        2 => args[1].clone(),
        _ => ScmVal::new_int(0),
    };

    Ok(ScmVal::new_vec_mut(vec![fill; size as usize]))
}

pub fn vector_set(args: &[ScmVal]) -> ValResult {
    let index = to_index(&args[1]).ok_or(ScmErr::BadArgType(
        "vector-set!".to_owned(),
        "exact non-negative integer".to_owned(),
        args[1].clone(),
    ))?;

    match args[0].clone() {
        ScmVal::NewVec(vec) => {
            if !vec.set(args[2].clone(), index) {
                return Err(ScmErr::OutOfBounds(index, vec.len()));
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

pub fn vector_ref(args: &[ScmVal]) -> ValResult {
    let index = to_index(&args[1]).ok_or(ScmErr::BadArgType(
        "vector-ref".to_owned(),
        "exact non-negative integer".to_owned(),
        args[1].clone(),
    ))?;

    match args[0].clone() {
        ScmVal::NewVec(vec) => Ok(vec.get(index).ok_or(ScmErr::RangeError(
            "vector-ref".to_owned(),
            args[1].clone(),
            args[0].clone(),
        ))?),
        _ => Err(ScmErr::BadArgType(
            "vector-ref".to_owned(),
            "vector".to_owned(),
            args[0].clone(),
        )),
    }
}

// Errors /////////////////////////////////////////////////////////////////////

fn user_error(args: &[ScmVal]) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("error!".to_owned(), 2));
    }

    Err(ScmErr::UserError(
        args[0].to_string(),
        args[1].to_string(),
        args[2..].into(),
    ))
}

fn user_arg_type_error(args: &[ScmVal]) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("arg-type-error!".to_owned(), 3));
    }

    Err(ScmErr::BadArgType(
        args[0].to_string(),
        args[2].to_string(),
        args[1].clone(),
    ))
}

fn user_range_error(args: &[ScmVal]) -> ValResult {
    if args.len() < 3 {
        return Err(ScmErr::Arity("range-error!".to_owned(), 3));
    }

    Err(ScmErr::RangeError(
        args[0].to_string(),
        args[1].clone(),
        args[2].clone(),
    ))
}

// Helpers ////////////////////////////////////////////////////////////////////

pub fn to_index(val: &ScmVal) -> Option<usize> {
    match val {
        ScmVal::Number(Num::Int(i)) if *i >= 0 => Some(*i as usize),
        _ => None,
    }
}
