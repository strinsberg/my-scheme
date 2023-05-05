use crate::error::{ScmErr, ValResult};
use crate::types::{Builtin, ListValIter, ScmVal};

// All builtin functions that are not syntactic keywords and are the basic building
// blocks for all other functions. Syntactic keywords and things that require tail
// call optimization or also return an environment will be in eval_tco as they are
// special.

// TODO add tests if you can, even though most things are, or can be tested
// by the core_procedure tests, though then other failures can break them.

// Helpers ////////////////////////////////////////////////////////////////////

// Only iterates long enough to check it meets arity, but would be better if cells
// were counted.
pub fn meets_arity(arg: ScmVal, arity: usize) -> Option<usize> {
    match arg {
        ScmVal::Pair(cell) => match ListValIter::new(cell).take(arity).count() {
            len if len == arity => Some(len),
            len => Some(len),
        },
        _ => None,
    }
}

// Apply Builtin //////////////////////////////////////////////////////////////

pub fn apply_core_proc(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    match op {
        Builtin::Cons => cons_proc(args),
        Builtin::Car => car_proc(args),
        Builtin::Cdr => cdr_proc(args),
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

pub fn cons_proc(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => Ok(ScmVal::new_pair(args[0].clone(), args[1].clone())),
        _ => Err(ScmErr::Arity("cons".to_owned(), 2)),
    }
}

pub fn car(arg: ScmVal) -> ValResult {
    match arg {
        ScmVal::Pair(cell) => Ok(cell.borrow().head.clone()),
        _ => Err(ScmErr::BadArgType("car".to_owned(), "pair".to_owned(), arg)),
    }
}

pub fn car_proc(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        1.. => car(args[0].clone()),
        _ => Err(ScmErr::Arity("car".to_owned(), 1)),
    }
}

pub fn cdr(arg: ScmVal) -> ValResult {
    match arg {
        ScmVal::Pair(cell) => Ok(cell.borrow().tail.clone()),
        _ => Err(ScmErr::BadArgType("cdr".to_owned(), "pair".to_owned(), arg)),
    }
}

pub fn cdr_proc(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        1.. => cdr(args[0].clone()),
        _ => Err(ScmErr::Arity("cdr".to_owned(), 1)),
    }
}

// Error here might not be good if list is not size 2
pub fn cadr(arg: ScmVal) -> ValResult {
    car(cdr(arg)?)
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
        _ => Err(ScmErr::Arity(format! {"{:?}", op}, 2)),
    }
}

fn binary_arithmetic(op: Builtin, args: Vec<ScmVal>) -> ValResult {
    let result;
    let left = args[0].clone();
    let right = args[1].clone();
    let type_str = "number".to_string();

    match left.clone() {
        ScmVal::Number(l) => match right.clone() {
            ScmVal::Number(r) => match op.clone() {
                Builtin::Sum => result = (l.add(r.clone()), l, r),
                Builtin::Subtract => result = (l.subtract(r.clone()), l, r),
                Builtin::Product => result = (l.multiply(r.clone()), l, r),
                Builtin::Divide => result = (l.divide(r.clone()), l, r),
                _ => panic!("operation should be arithmetic procedure: {:?}", op),
            },
            e => return Err(ScmErr::BadArgType(format!("{}", op), type_str, e)),
        },
        e => return Err(ScmErr::BadArgType(format!("{}", op), type_str, e)),
    }

    match result.0 {
        Some(num) => Ok(ScmVal::Number(num)),
        None => Err(ScmErr::BadArithmetic(op, result.1, result.2)),
    }
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
