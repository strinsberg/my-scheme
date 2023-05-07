use crate::error::{ScmErr, ValResult};
use crate::number::ScmNumber;
use crate::types::{Builtin, Env, ScmVal};
use std::cell::RefMut;

// All builtin functions that are not syntactic keywords and are the basic building
// blocks for all other functions. Syntactic keywords and things that require tail
// call optimization or also return an environment will be in eval_tco as they are
// special.

// TODO add tests if you can, even though most things are, or can be tested
// by the core_procedure tests, though then other failures can break them.

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
        Builtin::IsEmpty => is_empty(args),
        // Vectors
        Builtin::MakeVec => make_vector(args),
        Builtin::Vector => vector(args),
        Builtin::VecSet => vector_set(args),
        Builtin::VecRef => vector_ref(args),
        Builtin::VecLen => vector_length(args),
        Builtin::VecToList => vector_to_list(args),
        Builtin::VecFill => vector_fill(args),
        _ => panic!("unsupported builtin: {:?}", op),
    }
}

// Helpers ////////////////////////////////////////////////////////////////////

pub fn is_core_proc(val: ScmVal) -> bool {
    match val {
        ScmVal::Core(b) => match b {
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
        ScmVal::Boolean(false) | ScmVal::Empty => false,
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
            ScmVal::Pair(cell) => Ok(cell.borrow().head.clone()),
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
            ScmVal::Pair(cell) => Ok(cell.borrow().tail.clone()),
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
        2.. => Ok(ScmVal::Boolean(args[0] == args[1])),
        _ => Err(ScmErr::Arity("eqv?".to_owned(), 2)),
    }
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
            ScmVal::Pair(_) | ScmVal::DottedPair(_) => Ok(ScmVal::Boolean(true)),
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
            ScmVal::Core(_) | ScmVal::Closure(_) => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("procedure?".to_owned(), 1))
    }
}

pub fn is_empty(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 1 {
        match args[0].clone() {
            ScmVal::Empty => Ok(ScmVal::Boolean(true)),
            _ => Ok(ScmVal::Boolean(false)),
        }
    } else {
        Err(ScmErr::Arity("null?".to_owned(), 1))
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

pub fn vector_to_list(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("vector->list".to_owned(), 2));
    }

    match args[0].clone() {
        ScmVal::VectorMut(vec) => Ok(ScmVal::vec_to_list(vec.borrow().clone(), ScmVal::Empty)),
        ScmVal::Vector(vec) => Ok(ScmVal::vec_to_list((*vec).clone(), ScmVal::Empty)),
        _ => Err(ScmErr::BadArgType(
            "vector->list".to_owned(),
            "vector".to_owned(),
            args[0].clone(),
        )),
    }
}

pub fn vector_fill(args: Vec<ScmVal>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("vector-fill".to_owned(), 2));
    }

    match args[0].clone() {
        ScmVal::VectorMut(vec) => {
            vec.borrow_mut()
                .iter_mut()
                .for_each(|x| *x = args[1].clone());
        }
        _ => {
            return Err(ScmErr::BadArgType(
                "vector-fill!".to_owned(),
                "mutable vector".to_owned(),
                args[0].clone(),
            ))
        }
    };

    Ok(ScmVal::Empty)
}
