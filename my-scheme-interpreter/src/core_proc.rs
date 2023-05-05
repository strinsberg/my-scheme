use crate::error::{ScmErr, ValResult};
use crate::types::{Builtin, ScmVal};

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
        Builtin::BaseEnv => Ok(ScmVal::null_env()),
        Builtin::Sum | Builtin::Subtract | Builtin::Product | Builtin::Divide => {
            arithmetic_proc(op, args)
        }
        _ => panic!("unsupported builtin: {:?}", op),
    }
}

pub fn is_core_proc(val: ScmVal) -> bool {
    match val {
        ScmVal::Core(b) => match b {
            Builtin::Cons
            | Builtin::Car
            | Builtin::Cdr
            | Builtin::Sum
            | Builtin::Subtract
            | Builtin::Product
            | Builtin::Divide
            | Builtin::BaseEnv
            | Builtin::EQ
            | Builtin::Eqv => true,
            _ => false,
        },
        _ => false,
    }
}

// Core list functions ////////////////////////////////////////////////////////

pub fn cons(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => Ok(ScmVal::new_pair(args[0].clone(), args[1].clone())),
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

    Ok(ScmVal::Number(
        result.ok_or(ScmErr::BadArithmetic(op, left, right))?,
    ))
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

pub fn is_closure(val: ScmVal) -> bool {
    match val {
        ScmVal::Closure(_) => true,
        _ => false,
    }
}
