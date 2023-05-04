use crate::core_proc as proc;
use crate::env;
use crate::heap::Heap;
use crate::scm_types::builtin::Builtin;
use crate::scm_types::error::{ScmErr, ScmResult, TcoResult, ValResult};
use crate::scm_types::scm_val::{Closure, Pointer, ScmVal};
use crate::utils;
use std::iter::zip;

// TODO all have arity checks, but they have 2 different types. Try to make them
// the same, use the vec way.
//
// TODO it is an ERROR to use define within a form, so we should check for
// that somehow. Perhaps using eval_forms to check for define on the car
// of the expression to evaluate it properly, but define not being checked in
// eval or being an error if found.
//
// TODO lambda and define can take differnt forms than the ones I have
// (lambda (a b c) body) and (lambda x body)
// (define f arg),
// (define (f a b c) body) => (define f (lambda (a b c) body))
// (define (f . x) body) => (define f (lambda x body))
//
// TODO Check on how apply is supposed to work? For mine right now
// the arguments are evaluated, but the new call is returned unevaluated for tco,
// though I am not sure this does anything. The standard says the second arg should
// be evaluated with tco, but I am not sure if this means evaluating it to a
// function or the application of it. All evaluations use tco, so if it is a
// recursive function the closure application will be tco.
//
// TODO make sure everything in here is tested by the core_procedure.rs tests.
// It is good to test these with unit tests, but it is messy and difficult to
// setup the forms to eval and to expect. It is easier just to test with the
// output, even if other things could break the tests.

// Eval ///////////////////////////////////////////////////////////////////////

pub fn eval_tco(value: ScmVal, environment: Pointer, heap: &mut Heap) -> ValResult {
    //println!("{}", utils::val_to_string(value.clone(), heap));
    let mut expr = value.clone();
    let mut env = environment.clone();

    loop {
        return match expr.clone() {
            // Symbols get looked up and their values returned
            ScmVal::Symbol(_) => env::lookup(expr.clone(), env, heap)
                .ok_or(ScmErr::Undeclared(utils::extract_symbol_name(expr))),
            // Lists have their first arg applied to the cdr
            ScmVal::Pair(ptr) => {
                let cell = heap.get_cell(ptr);

                // Eval/Apply things that do not require evaluating the first element
                if cell.is_dotted() {
                    return Err(ScmErr::BadCall(utils::val_to_string(expr, heap)));
                } else if cell.head == utils::new_sym("quote") {
                    return proc::car(cell.tail, heap);
                } else if cell.head == utils::new_sym("lambda") {
                    return eval_lambda(cell.tail, env, heap);
                } else if cell.head == utils::new_sym("set!") {
                    return eval_set(cell.tail, env, heap);
                } else if cell.head == utils::new_sym("define") {
                    return eval_define(cell.tail, env, heap);
                } else if cell.head == utils::new_sym("if") {
                    expr = eval_if(cell.tail, env, heap)?;
                    continue;
                } else if cell.head == utils::new_sym("let") {
                    expr = eval_let(cell.tail, env, heap)?;
                    continue;
                } else if cell.head == utils::new_sym("letrec") {
                    expr = eval_letrec(cell.tail, heap)?;
                    continue;
                }

                // Eval the list elements
                let proc = eval_tco(cell.head.clone(), env, heap)?;
                let arg_vec = eval_vec(utils::list_to_vec(cell.tail, heap), env, heap)?;

                // Eval things that we can evaluate the first element into a proc
                if proc == ScmVal::Core(Builtin::Apply) {
                    expr = user_apply(arg_vec, heap)?;
                    continue;
                } else if proc == ScmVal::Core(Builtin::Eval) {
                    (expr, env) = user_eval(arg_vec)?;
                    continue;
                } else if proc::is_closure(proc.clone()) {
                    (expr, env) = apply_closure(proc.clone(), arg_vec, heap)?;
                    continue;
                } else if proc::is_core_proc(proc.clone()) {
                    match proc {
                        ScmVal::Core(op) => proc::apply_core_proc(op, arg_vec, heap),
                        _ => panic!("should be ScmVal::Core: {:?}", proc),
                    }
                } else {
                    Err(ScmErr::BadCall(utils::val_to_string(proc, heap)))
                }
            }
            // String, Bool, Char, Vector, Closure, Procedure all eval to themselves
            _ => Ok(expr),
        };
    }
}

// Evaluates a vector of forms and returns the value of the last one.
pub fn eval_forms(forms: Vec<ScmVal>, env: Pointer, heap: &mut Heap) -> ValResult {
    if forms.len() < 1 {
        return Ok(ScmVal::Empty);
    }

    for f in forms[..forms.len() - 1].iter() {
        eval_tco(f.clone(), env, heap)?;
    }
    eval_tco(forms[forms.len() - 1].clone(), env, heap)
}

// Eval Helpers ///////////////////////////////////////////////////////////////

// Evaluates an if statement with tco.
// Returns the unevaluated expression for the true or false branch based on the
// evaluation of the condition expression.
// Any branch past the false branch is ignored.
pub fn eval_if(args: ScmVal, env: Pointer, heap: &mut Heap) -> ValResult {
    match proc::meets_arity(args.clone(), 3, heap) {
        // (if cond true false)
        Some(3) => eval_if_helper(args, true, env, heap),
        // (if cond true)
        Some(2) => eval_if_helper(args, false, env, heap),
        // has empty args or only cond
        Some(_) => Err(ScmErr::Arity("if".to_owned(), 2)),
        None => panic!("if args should be a pair: {:?}", args),
    }
}

// Helper to evaluate properly when given a false branch or not
fn eval_if_helper(args: ScmVal, false_branch: bool, env: Pointer, heap: &mut Heap) -> ValResult {
    match args {
        ScmVal::Pair(ptr) => {
            // check condition
            let cond = heap.car(ptr);
            let result = eval_tco(cond, env, heap)?;
            // return the correct branch unevaluated
            match result {
                ScmVal::Boolean(false) | ScmVal::Empty => {
                    if false_branch {
                        heap.nth(ptr, 2)
                    } else {
                        Ok(ScmVal::Empty)
                    }
                }
                _ => heap.nth(ptr, 1),
            }
        }
        _ => panic!("if args should be a pair: {:?}", args),
    }
}

// Evaluates a lambda statment into a closure and returns it.
pub fn eval_lambda(args: ScmVal, env: Pointer, heap: &mut Heap) -> ValResult {
    // We are sure it is a pair?
    let arg_vec = utils::list_to_vec(args.clone(), heap);
    match arg_vec.len() {
        2.. => {
            let params = arg_vec[0].clone();
            let params_vec = match params {
                ScmVal::Pair(_) => utils::list_to_vec(params, heap),
                ScmVal::Empty => vec![],
                _ => {
                    return Err(ScmErr::BadArgType(
                        "lambda".to_owned(),
                        1,
                        "(pair ...)".to_owned(),
                    ))
                }
            };

            let body_vec: Vec<ScmVal> = arg_vec[1..].into();
            match body_vec.len() {
                0 => Err(ScmErr::EmptyBody),
                _ => Ok(utils::new_closure(
                    Closure::new(env, params_vec, body_vec),
                    heap,
                )?),
            }
        }
        _ => Err(ScmErr::Arity("lambda".to_owned(), 2)),
    }
}

// Converts a let statment into a lambda statment and returns a list
// with the evaluated clojure and its arguments.
// Example: (let ((a 1) (b 2)) (+ a b)) => ((lambda (a b) (+ a b)) 1 2)
pub fn eval_let(args: ScmVal, env: Pointer, heap: &mut Heap) -> ValResult {
    match proc::meets_arity(args.clone(), 2, heap) {
        Some(2) => {
            let bindings = proc::car(args.clone(), heap)?;
            let body = proc::cdr(args.clone(), heap)?;
            let (params, bind_args) = match unbind(bindings.clone(), heap) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(utils::val_to_string(bindings, heap))),
            };
            // Get the closure and build the new function call expr
            let lambda = eval_lambda(proc::cons(params, body.clone(), heap)?, env, heap)?;
            proc::cons(lambda, bind_args, heap)
        }
        Some(_) => Err(ScmErr::Arity("let".to_owned(), 2)),
        None => panic!("let args should be a pair: {:?}", args),
    }
}

// Converts a letrec statment into a let statment.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn eval_letrec(args: ScmVal, heap: &mut Heap) -> ValResult {
    match proc::meets_arity(args.clone(), 2, heap) {
        Some(2) => {
            // Destructure letrec
            let bindings = proc::car(args.clone(), heap)?;
            let body = proc::cdr(args.clone(), heap)?;

            // Make new binding list with <undefined> and list of set! assignments
            let (bind_vec, assign_vec) = match letrec_bind(bindings.clone(), heap) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(utils::val_to_string(bindings, heap))),
            };

            // Make the new let expr and return it
            let new_body: Vec<ScmVal> = assign_vec
                .into_iter()
                .chain(utils::list_to_vec(body.clone(), heap).into_iter())
                .collect();
            let rest = proc::cons(
                utils::vec_to_list(bind_vec, heap),
                utils::vec_to_list(new_body, heap),
                heap,
            )?;
            Ok(proc::cons(utils::new_sym("let"), rest, heap)?)
        }
        Some(_) => Err(ScmErr::Arity("lambda".to_owned(), 2)),
        None => panic!("lambda args should be a pair: {:?}", args),
    }
}

// Sets the value first element of the pair to the evaluation of
// the second element in the env.
pub fn eval_set(pair: ScmVal, env: Pointer, heap: &mut Heap) -> ValResult {
    match proc::meets_arity(pair.clone(), 2, heap) {
        Some(2) => {
            let key = proc::car(pair.clone(), heap)?;
            let val = eval_tco(proc::cadr(pair.clone(), heap)?, env, heap)?;
            env::set(key.clone(), val.clone(), env, heap)
                .ok_or(ScmErr::Undeclared(utils::val_to_string(key, heap)))
        }
        Some(_) => Err(ScmErr::Arity("set!".to_owned(), 2)),
        None => panic!("set! args should be a pair: {:?}", pair),
    }
}

// Evaluate a define statement by first setting the variable to <undefined>
// and then using set! to store the new evaluated value.
pub fn eval_define(args: ScmVal, env: Pointer, heap: &mut Heap) -> ValResult {
    match proc::meets_arity(args.clone(), 2, heap) {
        Some(2) => {
            let var = proc::car(args.clone(), heap)?;
            env::insert(var.clone(), ScmVal::Undefined, env, heap);
            eval_set(args, env, heap)
        }
        Some(_) => Err(ScmErr::Arity("define".to_owned(), 2)),
        None => panic!("define args should be a pair: {:?}", args),
    }
}

// Apply //////////////////////////////////////////////////////////////////////

// Apply the function that a closure represents evaluating its body with the
// captured environment.
pub fn apply_closure(val: ScmVal, args: Vec<ScmVal>, heap: &mut Heap) -> TcoResult {
    let closure = utils::extract_closure(val, heap);
    let arity = closure.params.len();

    if args.len() >= arity {
        // bind params and args to the captured env (args are already evalled)
        let bound_env = env::bind(closure.params, args, closure.env, heap);

        // eval the body expressions excluding the one in tail position
        for exp in &closure.body[..closure.body.len() - 1] {
            eval_tco(exp.clone(), bound_env, heap)?;
        }

        // Return the tail expression unevaluated and the captured environment for
        // subsequent evaluation with tco
        Ok((closure.body[closure.body.len() - 1].clone(), bound_env))
    } else {
        Err(ScmErr::Arity("closure".to_owned(), 2))
    }
}

// Apply a function to a list of values.
// Only works right now if the second arg is a list and ignores additional args.
// Not sure if this is the intended r5rs behaviour.
pub fn user_apply(args: Vec<ScmVal>, heap: &mut Heap) -> ValResult {
    match args.len() {
        2.. => {
            let func = args[0].clone();
            let list = match args[1].clone() {
                ScmVal::Pair(p) => ScmVal::Pair(p),
                _ => return Err(ScmErr::BadArgType("apply".to_owned(), 2, "pair".to_owned())),
            };
            proc::cons(func, list, heap)
        }
        _ => Err(ScmErr::Arity("apply".to_owned(), 2)),
    }
}

// Evaluates the first argument using a user supplied environment.
pub fn user_eval(args: Vec<ScmVal>) -> TcoResult {
    match args.len() {
        2.. => {
            let expr = args[0].clone();
            match args[1].clone() {
                ScmVal::Env(ptr) => Ok((expr, ptr)),
                _ => Err(ScmErr::BadArgType(
                    "eval".to_owned(),
                    2,
                    "environment".to_owned(),
                )),
            }
        }
        _ => Err(ScmErr::Arity("apply".to_owned(), 2)),
    }
}

// Helpers ////////////////////////////////////////////////////////////////////

fn eval_vec(v: Vec<ScmVal>, env: Pointer, heap: &mut Heap) -> ScmResult<Vec<ScmVal>> {
    let evalled: Result<Vec<ScmVal>, _> =
        v.into_iter().map(|val| eval_tco(val, env, heap)).collect();
    Ok(evalled?)
}

fn unbind(list: ScmVal, heap: &mut Heap) -> Option<(ScmVal, ScmVal)> {
    unbind_help(list, heap).map(|(param_vec, arg_vec)| {
        (
            utils::vec_to_list(param_vec, heap),
            utils::vec_to_list(arg_vec, heap),
        )
    })
}

fn unbind_help(list: ScmVal, heap: &mut Heap) -> Option<(Vec<ScmVal>, Vec<ScmVal>)> {
    match list {
        ScmVal::Pair(_) => {
            let pairs: Vec<Vec<ScmVal>> = utils::list_to_vec(list, heap)
                .into_iter()
                .map(|p| utils::list_to_vec(p, heap))
                .collect(); // now pairs is vec![vec[ScmVal, ScmVal], ...]

            let mut params = Vec::new();
            let mut args = Vec::new();
            for p in pairs.iter() {
                if p.len() >= 2 {
                    params.push(p[0].clone());
                    args.push(p[1].clone());
                } else {
                    return None;
                }
            }

            Some((params, args))
        }
        _ => return None,
    }
}

fn letrec_bind(list: ScmVal, heap: &mut Heap) -> Option<(Vec<ScmVal>, Vec<ScmVal>)> {
    let (params, args) = unbind_help(list, heap)?;

    // Create let bindings to declare the vars ((val1 <undefined>) ...)
    let bindings = params
        .clone()
        .into_iter()
        .map(|p| ScmVal::Pair(heap.cons(p, ScmVal::Undefined)))
        .collect();

    // Create the assingments to add to the start of the body
    // ((set! val1 arg1) ...)
    let assignments = zip(params.clone(), args.clone())
        .map(|(p, a)| {
            let begin = ScmVal::Pair(heap.cons(a, ScmVal::Empty));
            let rest = ScmVal::Pair(heap.cons(p, begin));
            ScmVal::Pair(heap.cons(utils::new_sym("set!"), rest))
        })
        .collect();

    Some((bindings, assignments))
}
