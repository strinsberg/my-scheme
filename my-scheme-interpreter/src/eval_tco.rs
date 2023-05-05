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
                let args = ScmVal::list_to_vec(cell.borrow().tail.clone());

                // Eval/Apply things that do not require evaluating the first element
                if dotted {
                    return Err(ScmErr::Syntax(expr));
                } else if cell.borrow().head == ScmVal::new_sym("quote") {
                    return Ok(args[0].clone());
                } else if cell.borrow().head == ScmVal::new_sym("lambda") {
                    return eval_lambda(args, Rc::clone(&env));
                } else if cell.borrow().head == ScmVal::new_sym("set!") {
                    return eval_set(args, env);
                } else if cell.borrow().head == ScmVal::new_sym("define") {
                    return eval_define(args, env);
                } else if cell.borrow().head == ScmVal::new_sym("if") {
                    expr = eval_if(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("let") {
                    expr = eval_let(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("letrec") {
                    expr = eval_letrec(args)?;
                    continue;
                }

                // Eval the list elements
                let proc = eval_tco(cell.borrow().head.clone(), Rc::clone(&env))?;
                let arg_vec = eval_vec(args, Rc::clone(&env))?;

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
                    Err(ScmErr::Syntax(expr))
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
pub fn eval_if(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    match args.len() {
        // (if cond true false)
        3.. => eval_if_helper(args, true, env),
        // (if cond true)
        2 => eval_if_helper(args, false, env),
        // has empty args or only cond
        _ => Err(ScmErr::Arity("if".to_owned(), 2)),
    }
}

// Helper to evaluate properly when given a false branch or not
fn eval_if_helper(args: Vec<ScmVal>, false_branch: bool, env: Rc<RefCell<Env>>) -> ValResult {
    // check condition
    let result = eval_tco(args[0].clone(), env)?;
    // return the correct branch unevaluated
    match result {
        ScmVal::Boolean(false) | ScmVal::Empty => {
            if false_branch {
                Ok(args[2].clone())
            } else {
                Ok(ScmVal::Empty)
            }
        }
        _ => Ok(args[1].clone()),
    }
}

// Evaluates a lambda statment into a closure and returns it.
pub fn eval_lambda(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    match args.len() {
        2.. => {
            let params = args[0].clone();
            let params_vec = match params {
                ScmVal::Pair(_) => ScmVal::list_to_vec(params),
                ScmVal::Empty => vec![],
                e => {
                    return Err(ScmErr::BadArgType(
                        "lambda".to_owned(),
                        "list of symbols".to_owned(),
                        e,
                    ))
                }
            };

            let body_vec: Vec<ScmVal> = args[1..].into();
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
pub fn eval_let(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    match args.len() {
        2.. => {
            let bindings = args[0].clone();
            let body: Vec<ScmVal> = args[1..].into();
            let (params, bind_args) = match unbind(bindings.clone()) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(bindings.to_string())),
            };
            match body.len() {
                0 => Err(ScmErr::EmptyBody),
                _ => Ok(ScmVal::new_pair(
                    ScmVal::new_closure(Closure::new(env, params, body)),
                    ScmVal::vec_to_list(ScmVal::Empty, bind_args),
                )),
            }
        }
        _ => Err(ScmErr::Arity("let".to_owned(), 2)),
    }
}

// Converts a letrec statment into a let statment.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn eval_letrec(args: Vec<ScmVal>) -> ValResult {
    match args.len() {
        2.. => {
            // Destructure letrec
            let bindings = args[0].clone();
            let body: Vec<ScmVal> = args[1..].into();

            // Make new binding list with <undefined> and list of set! assignments
            let (bind_vec, assign_vec) = match letrec_bind(bindings.clone()) {
                Some(pair) => pair,
                None => return Err(ScmErr::BadBindings(bindings.to_string())),
            };

            // Make the new let expr and return it
            let new_body: Vec<ScmVal> = assign_vec.iter().chain(body.iter()).cloned().collect();
            let rest = ScmVal::new_pair(
                ScmVal::vec_to_list(ScmVal::Empty, bind_vec),
                ScmVal::vec_to_list(ScmVal::Empty, new_body),
            );
            Ok(ScmVal::new_pair(ScmVal::new_sym("let"), rest))
        }
        _ => Err(ScmErr::Arity("lambda".to_owned(), 2)),
    }
}

// Sets the value first element of the pair to the evaluation of
// the second element in the env.
pub fn eval_set(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    match args.len() {
        2.. => {
            let key = args[0].clone();
            let val = eval_tco(args[1].clone(), Rc::clone(&env))?;
            env.borrow_mut()
                .set(key.clone(), val.clone())
                .ok_or(ScmErr::Undeclared(key.to_string()))
        }
        _ => Err(ScmErr::Arity("set!".to_owned(), 2)),
    }
}

// Evaluate a define statement by first setting the variable to <undefined>
// and then using set! to store the new evaluated value.
pub fn eval_define(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    match args.len() {
        2.. => {
            {
                env.borrow_mut().insert(args[0].clone(), ScmVal::Undefined);
            }
            eval_set(args, env)
        }
        _ => Err(ScmErr::Arity("define".to_owned(), 2)),
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
                e => return Err(ScmErr::BadArgType("apply".to_owned(), "pair".to_owned(), e)),
            };
            Ok(ScmVal::new_pair(func, list))
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
                ScmVal::Env(env) => Ok((expr, env)),
                e => Err(ScmErr::BadArgType(
                    "eval".to_owned(),
                    "environment".to_owned(),
                    e,
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

fn unbind(list: ScmVal) -> Option<(Vec<ScmVal>, Vec<ScmVal>)> {
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
    let (params, args) = unbind(list)?;

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
