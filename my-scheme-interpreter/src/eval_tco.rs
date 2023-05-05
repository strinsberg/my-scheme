use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, TcoResult, ValResult};
use crate::types::{Builtin, Closure, Env, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

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

pub fn eval_tco(value: ScmVal, environment: Rc<RefCell<Env>>) -> ValResult {
    //println!("{}", value.clone().to_string());
    let mut expr = value.clone();
    let mut env = Rc::clone(&environment);

    loop {
        return match expr.clone() {
            // Symbols get looked up and their values returned
            ScmVal::Symbol(name) => env
                .borrow()
                .lookup(expr.clone())
                .ok_or(ScmErr::Undeclared(name.to_string())),
            // Lists have their first arg applied to the cdr
            ScmVal::Pair(cell) => {
                let dotted = cell.borrow().is_dotted();

                // Eval/Apply things that do not require evaluating the first element
                if dotted {
                    return Err(ScmErr::BadCall(expr.to_string()));
                } else if cell.borrow().head == ScmVal::new_sym("quote") {
                    return proc::car(cell.borrow().tail.clone());
                } else if cell.borrow().head == ScmVal::new_sym("lambda") {
                    return eval_lambda(cell.borrow().tail.clone(), Rc::clone(&env));
                } else if cell.borrow().head == ScmVal::new_sym("set!") {
                    return eval_set(cell.borrow().clone().tail, env);
                } else if cell.borrow().head == ScmVal::new_sym("define") {
                    return eval_define(cell.borrow().clone().tail, env);
                } else if cell.borrow().head == ScmVal::new_sym("if") {
                    expr = eval_if(cell.borrow().clone().tail, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("let") {
                    expr = eval_let(cell.borrow().clone().tail, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("letrec") {
                    expr = eval_letrec(cell.borrow().clone().tail)?;
                    continue;
                }

                // Eval the list elements
                let proc = eval_tco(cell.borrow().head.clone(), Rc::clone(&env))?;
                let arg_vec = eval_vec(
                    ScmVal::list_to_vec(cell.borrow().tail.clone()),
                    Rc::clone(&env),
                )?;

                // Eval things that we can evaluate the first element into a proc
                if proc == ScmVal::Core(Builtin::Apply) {
                    expr = user_apply(arg_vec)?;
                    continue;
                } else if proc == ScmVal::Core(Builtin::Eval) {
                    (expr, env) = user_eval(arg_vec)?;
                    continue;
                } else if proc::is_closure(proc.clone()) {
                    (expr, env) = apply_closure(proc.clone(), arg_vec)?;
                    continue;
                } else if proc::is_core_proc(proc.clone()) {
                    match proc {
                        ScmVal::Core(op) => proc::apply_core_proc(op, arg_vec),
                        _ => panic!("should be ScmVal::Core: {:?}", proc),
                    }
                } else {
                    Err(ScmErr::BadCall(proc.to_string()))
                }
            }
            // String, Bool, Char, Vector, Closure, Procedure all eval to themselves
            _ => Ok(expr),
        };
    }
}

// Evaluates a vector of forms and returns the value of the last one.
pub fn eval_forms(forms: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if forms.len() < 1 {
        return Ok(ScmVal::Empty);
    }

    for f in forms[..forms.len() - 1].iter() {
        eval_tco(f.clone(), Rc::clone(&env))?;
    }
    eval_tco(forms[forms.len() - 1].clone(), env)
}

// Eval Helpers ///////////////////////////////////////////////////////////////

// Evaluates an if statement with tco.
// Returns the unevaluated expression for the true or false branch based on the
// evaluation of the condition expression.
// Any branch past the false branch is ignored.
pub fn eval_if(args: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    match proc::meets_arity(args.clone(), 3) {
        // (if cond true false)
        Some(3) => eval_if_helper(args, true, env),
        // (if cond true)
        Some(2) => eval_if_helper(args, false, env),
        // has empty args or only cond
        Some(_) => Err(ScmErr::Arity("if".to_owned(), 2)),
        None => panic!("if args should be a pair: {:?}", args),
    }
}

// Helper to evaluate properly when given a false branch or not
fn eval_if_helper(args: ScmVal, false_branch: bool, env: Rc<RefCell<Env>>) -> ValResult {
    match args {
        ScmVal::Pair(_) => {
            // check condition
            let result = eval_tco(proc::car(args.clone())?, env)?;
            // return the correct branch unevaluated
            match result {
                ScmVal::Boolean(false) | ScmVal::Empty => {
                    if false_branch {
                        proc::car(proc::cdr(proc::cdr(args)?)?)
                    } else {
                        Ok(ScmVal::Empty)
                    }
                }
                _ => proc::cadr(args),
            }
        }
        _ => panic!("if args should be a pair: {:?}", args),
    }
}

// Evaluates a lambda statment into a closure and returns it.
pub fn eval_lambda(args: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    // We are sure it is a pair?
    let arg_vec = ScmVal::list_to_vec(args);
    match arg_vec.len() {
        2.. => {
            let params = arg_vec[0].clone();
            let params_vec = match params {
                ScmVal::Pair(_) => ScmVal::list_to_vec(params),
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
                _ => Ok(ScmVal::new_closure(Closure::new(env, params_vec, body_vec))),
            }
        }
        _ => Err(ScmErr::Arity("lambda".to_owned(), 2)),
    }
}

// Converts a let statment into a lambda statment and returns a list
// with the evaluated clojure and its arguments.
// Example: (let ((a 1) (b 2)) (+ a b)) => ((lambda (a b) (+ a b)) 1 2)
pub fn eval_let(args: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    match proc::meets_arity(args.clone(), 2) {
        Some(2) => {
            let bindings = proc::car(args.clone())?;
            let body = proc::cdr(args.clone())?;
            let (params, bind_args) = match unbind(bindings.clone()) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(bindings.to_string())),
            };
            // Get the closure and build the new function call expr
            let lambda = eval_lambda(proc::cons(params, body.clone())?, env)?;
            proc::cons(lambda, bind_args)
        }
        Some(_) => Err(ScmErr::Arity("let".to_owned(), 2)),
        None => panic!("let args should be a pair: {:?}", args),
    }
}

// Converts a letrec statment into a let statment.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn eval_letrec(args: ScmVal) -> ValResult {
    match proc::meets_arity(args.clone(), 2) {
        Some(2) => {
            // Destructure letrec
            let bindings = proc::car(args.clone())?;
            let body = proc::cdr(args.clone())?;

            // Make new binding list with <undefined> and list of set! assignments
            let (bind_vec, assign_vec) = match letrec_bind(bindings.clone()) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(bindings.to_string())),
            };

            // Make the new let expr and return it
            let new_body: Vec<ScmVal> = assign_vec
                .into_iter()
                .chain(ScmVal::list_to_vec(body.clone()).into_iter())
                .collect();
            let rest = ScmVal::new_pair(
                ScmVal::vec_to_list(ScmVal::Empty, bind_vec),
                ScmVal::vec_to_list(ScmVal::Empty, new_body),
            );
            Ok(ScmVal::new_pair(ScmVal::new_sym("let"), rest))
        }
        Some(_) => Err(ScmErr::Arity("lambda".to_owned(), 2)),
        None => panic!("lambda args should be a pair: {:?}", args),
    }
}

// Sets the value first element of the pair to the evaluation of
// the second element in the env.
pub fn eval_set(pair: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    match proc::meets_arity(pair.clone(), 2) {
        Some(2) => {
            let key = proc::car(pair.clone())?;
            let val = eval_tco(proc::cadr(pair.clone())?, Rc::clone(&env))?;
            env.borrow_mut()
                .set(key.clone(), val.clone())
                .ok_or(ScmErr::Undeclared(key.to_string()))
        }
        Some(_) => Err(ScmErr::Arity("set!".to_owned(), 2)),
        None => panic!("set! args should be a pair: {:?}", pair),
    }
}

// Evaluate a define statement by first setting the variable to <undefined>
// and then using set! to store the new evaluated value.
pub fn eval_define(args: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    match proc::meets_arity(args.clone(), 2) {
        Some(2) => {
            let var = proc::car(args.clone())?;
            {
                env.borrow_mut().insert(var.clone(), ScmVal::Undefined);
            }
            eval_set(args, env)
        }
        Some(_) => Err(ScmErr::Arity("define".to_owned(), 2)),
        None => panic!("define args should be a pair: {:?}", args),
    }
}

// Apply //////////////////////////////////////////////////////////////////////

// Apply the function that a closure represents evaluating its body with the
// captured environment.
pub fn apply_closure(val: ScmVal, args: Vec<ScmVal>) -> TcoResult {
    let closure = match val {
        ScmVal::Closure(c) => c,
        _ => panic!("should be passed an ScmVal::Closure"),
    };
    let arity = closure.params.len();

    if args.len() >= arity {
        // bind params and args to the captured env (args are already evalled)
        let bound_env = Env::bind_in_new_env(Rc::clone(&closure.env), closure.params.clone(), args);

        // eval the body expressions excluding the one in tail position
        for expr in &closure.body[..closure.body.len() - 1] {
            eval_tco(expr.clone(), Rc::clone(&bound_env))?;
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
pub fn user_apply(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => {
            let func = args[0].clone();
            let list = match args[1].clone() {
                ScmVal::Pair(p) => ScmVal::Pair(p),
                _ => return Err(ScmErr::BadArgType("apply".to_owned(), 2, "pair".to_owned())),
            };
            proc::cons(func, list)
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

fn eval_vec(v: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ScmResult<Vec<ScmVal>> {
    let evalled: Result<Vec<ScmVal>, _> = v
        .into_iter()
        .map(|val| eval_tco(val, Rc::clone(&env)))
        .collect();
    Ok(evalled?)
}

fn unbind(list: ScmVal) -> Option<(ScmVal, ScmVal)> {
    unbind_help(list).map(|(param_vec, arg_vec)| {
        (
            ScmVal::vec_to_list(ScmVal::Empty, param_vec),
            ScmVal::vec_to_list(ScmVal::Empty, arg_vec),
        )
    })
}

fn unbind_help(list: ScmVal) -> Option<(Vec<ScmVal>, Vec<ScmVal>)> {
    match list {
        ScmVal::Pair(_) => {
            let pairs: Vec<Vec<ScmVal>> = ScmVal::list_to_vec(list)
                .into_iter()
                .map(|p| ScmVal::list_to_vec(p))
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

fn letrec_bind(list: ScmVal) -> Option<(Vec<ScmVal>, Vec<ScmVal>)> {
    let (params, args) = unbind_help(list)?;

    // Create let bindings to declare the vars ((val1 <undefined>) ...)
    let bindings = params
        .clone()
        .into_iter()
        .map(|p| ScmVal::new_pair(p, ScmVal::Undefined))
        .collect();

    // Create the assingments to add to the start of the body
    // ((set! val1 arg1) ...)
    let assignments = zip(params.clone(), args.clone())
        .map(|(p, a)| {
            let begin = ScmVal::new_pair(a, ScmVal::Empty);
            let rest = ScmVal::new_pair(p, begin);
            ScmVal::new_pair(ScmVal::new_sym("set!"), rest)
        })
        .collect();

    Some((bindings, assignments))
}
