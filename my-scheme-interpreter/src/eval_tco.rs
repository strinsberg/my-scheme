use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, TcoResult, ValResult};
use crate::types::{Builtin, Closure, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

// TODO vector constants must be quoted, so like dotted lists it is an error to
// use them directly.
// TODO both vector constants and string literals are immutable and cannot be
// set, so they must be updated in the Reader if that is not the case already.
// TODO look into the pair dotted pair thing more, but some testing suggest that
// set-cdr! cannot turn a long dotted list or regular list into the other type.
// though the standard seems to suggest that a list can change if you set the
// fist cells cdr to a value. The big problem here is that even if list? does not
// change it's idea about the type of list the printing does show it as expected,
// which tells me that we still have to care about is_dotted() no matter what we
// start with.

// TODO the user_eval should take the current environment when called with no args...
// or whatever the standard says. It says there are 3 envs, the one that would
// get used when called with no args is the interaction-environment and I see that
// as the env that is already present. The expectation is that if eval allows
// define expressions that when using the other two you cannot add definitions
// to them. So if they are shared they have to be immutable environments and if
// they are just copies, like they are now, then they can be whatever I have
// and will not alter the environment outside of the evaluation. I expect that
// for now making a fresh env with null-environment would be best to keep
// anything from bleeding into the base env when it is not supposed to.

// Eval ///////////////////////////////////////////////////////////////////////

pub fn eval_tco(value: ScmVal, environment: Rc<RefCell<Env>>, top: bool) -> ValResult {
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
            ScmVal::DottedPair(_) => Err(ScmErr::Syntax(expr)),
            ScmVal::Pair(cell) => {
                let args = ScmVal::list_to_vec(cell.borrow().tail.clone())
                    .expect("should be unreachable if cell.is_dotted() was checked");

                if cell.borrow().head == ScmVal::new_sym("quote") {
                    return Ok(args[0].clone());
                } else if cell.borrow().head == ScmVal::new_sym("lambda") {
                    return eval_lambda(args, Rc::clone(&env));
                } else if cell.borrow().head == ScmVal::new_sym("set!") {
                    return eval_set(args, env);
                } else if cell.borrow().head == ScmVal::new_sym("define") {
                    if top {
                        return eval_define(args, env);
                    } else {
                        return Err(ScmErr::Syntax(expr));
                    }
                } else if cell.borrow().head == ScmVal::new_sym("if") {
                    expr = eval_if(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("let") {
                    expr = eval_let(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("let*") {
                    expr = eval_let_star(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("letrec") {
                    expr = eval_letrec(args)?;
                    continue;
                //  other derived expression, would be best to use macros, but they
                //  are not implemented yet.
                } else if cell.borrow().head == ScmVal::new_sym("and") {
                    expr = eval_and(args, Rc::clone(&env))?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("or") {
                    let (result, do_tco) = eval_or(args, Rc::clone(&env));
                    if do_tco {
                        expr = result?;
                        continue;
                    } else {
                        return result;
                    }
                } else if cell.borrow().head == ScmVal::new_sym("do") {
                    expr = eval_do(args)?;
                    continue;
                } else if cell.borrow().head == ScmVal::new_sym("begin") {
                    expr = eval_begin(args, Rc::clone(&env))?;
                    continue;
                }
                // case, cond, begin, named-let, delay, quasiquote(`)

                // Eval the list elements
                let proc = eval_tco(cell.borrow().head.clone(), Rc::clone(&env), false)?;
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
        eval_tco(f.clone(), Rc::clone(&env), true)?;
    }
    eval_tco(forms[forms.len() - 1].clone(), env, true)
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
    let result = eval_tco(args[0].clone(), env, false)?;
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
    if args.len() >= 2 {
        let params = args[0].clone();
        let formals = match params {
            ScmVal::Symbol(_) => Formals::Collect(params),
            ScmVal::Pair(_) => Formals::Fixed(ScmVal::list_to_vec(params).ok_or(
                ScmErr::BadArgType("lambda".to_owned(), "pair".to_owned(), args[0].clone()),
            )?),
            ScmVal::DottedPair(_) => {
                let vec = ScmVal::list_to_vec(params).ok_or(ScmErr::BadArgType(
                    "lambda".to_owned(),
                    "pair".to_owned(),
                    args[0].clone(),
                ))?;
                Formals::Rest(vec[..vec.len() - 1].into(), vec[vec.len() - 1].clone())
            }
            ScmVal::Empty => Formals::Fixed(vec![]),
            _ => {
                return Err(ScmErr::BadArgType(
                    "lambda".to_owned(),
                    "pair".to_owned(),
                    args[0].clone(),
                ))
            }
        };

        Ok(ScmVal::new_closure(Closure::new(
            env,
            formals,
            args[1..].into(),
        )))
    } else {
        Err(ScmErr::Arity("lambda".to_owned(), 2))
    }
}

// Converts a let statment into a lambda statment and returns a list
// with the evaluated clojure and its arguments.
// Example: (let ((a 1) (b 2)) (+ a b)) => ((lambda (a b) (+ a b)) 1 2)
pub fn eval_let(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let bindings = args[0].clone();
        let (params, bind_args) = unbind(bindings.clone())?;
        Ok(ScmVal::cons(
            ScmVal::new_closure(Closure::new(env, Formals::Fixed(params), args[1..].into())),
            ScmVal::vec_to_list(bind_args, ScmVal::Empty),
        ))
    } else {
        Err(ScmErr::Arity("let".to_owned(), 2))
    }
}

// It is easier not to convert this one into a million nested let expressions
// but instead look through the bindings and add them to the env one at a time
// before evaluating the body.
pub fn eval_let_star(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        // Get bindings list as a vector
        let bindings_vec = ScmVal::list_to_vec(args[0].clone()).ok_or(ScmErr::BadArgType(
            "let*".to_owned(),
            "pair".to_owned(),
            args[0].clone(),
        ))?;

        // Add a scope to the passed env to get a new env
        let new_env = Env::add_scope(env);

        // loop through the bindings and bind their evaluated value in the new env
        // sequentially. This makes it possible for a binding to reference previous
        // bindings, but does not allow for mutal recursion like letrec.
        for bind in bindings_vec.iter() {
            let bind_vec = ScmVal::list_to_vec(bind.clone()).ok_or(ScmErr::BadArgType(
                "let* binding".to_owned(),
                "pair".to_owned(),
                args[0].clone(),
            ))?;
            if bind_vec.len() < 2 {
                return Err(ScmErr::Arity("let* binding".to_owned(), 2));
            }

            let value = eval_tco(bind_vec[1].clone(), Rc::clone(&new_env), false)?;
            new_env.borrow_mut().insert(bind_vec[0].clone(), value)?;
        }

        // Put the new env into a closure that takes no arguments and call it with
        // not arguments. I.e. ((lambda () body))
        Ok(ScmVal::cons(
            ScmVal::new_closure(Closure::new(
                new_env,
                Formals::Fixed(vec![]),
                args[1..].into(),
            )),
            ScmVal::Empty,
        ))
    } else {
        Err(ScmErr::Arity("let".to_owned(), 2))
    }
}

// Converts a letrec statment into a let statment.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn eval_letrec(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 2 {
        let bindings = args[0].clone();
        let (bind_vec, assign_vec) = letrec_bind(bindings.clone())?;
        let new_body: Vec<ScmVal> = assign_vec.iter().chain(args[1..].iter()).cloned().collect();

        // Cons let and bindings onto the new body list
        Ok(ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("let"),
                ScmVal::vec_to_list(bind_vec, ScmVal::Empty),
            ],
            ScmVal::vec_to_list(new_body, ScmVal::Empty),
        ))
    } else {
        Err(ScmErr::Arity("lambda".to_owned(), 2))
    }
}

// Sets the value first element of the pair to the evaluation of
// the second element in the env.
pub fn eval_set(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let key = args[0].clone();
        let val = eval_tco(args[1].clone(), Rc::clone(&env), false)?;
        env.borrow_mut().set(key.clone(), val.clone())
    } else {
        Err(ScmErr::Arity("set!".to_owned(), 2))
    }
}

// Evaluate a define statement by first setting the variable to <undefined>
// and then using set! to store the new evaluated value.
pub fn eval_define(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("define".to_owned(), 2));
    }

    let first = args[0].clone();
    match first {
        ScmVal::Symbol(_) => {
            env.borrow_mut().insert(first.clone(), ScmVal::Undefined)?;
            eval_set(args, env)
        }
        ScmVal::Pair(cell) | ScmVal::DottedPair(cell) => {
            // just rebuild with (define name lambda) and re-evaluate
            let name = cell.borrow().head.clone();
            let params = cell.borrow().tail.clone();
            let body = ScmVal::vec_to_list(args[1..].into(), ScmVal::Empty);
            let lambda = ScmVal::cons(ScmVal::new_sym("lambda"), ScmVal::cons(params, body));
            eval_tco(
                ScmVal::vec_to_list(vec![ScmVal::new_sym("define"), name, lambda], ScmVal::Empty),
                env,
                true,
            )
        }
        _ => Err(ScmErr::BadArgType(
            "define".to_owned(),
            "pair or symbol".to_owned(),
            first,
        )),
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

    // Bind the arguments to their parameters according to the formals list
    let bound_env = match closure.params.clone() {
        Formals::Collect(symbol) => Env::bind_in_new_env(
            Rc::clone(&closure.env),
            vec![symbol],
            vec![ScmVal::vec_to_list(args, ScmVal::Empty)],
        )?,
        Formals::Fixed(params) => {
            if args.len() >= params.len() {
                Env::bind_in_new_env(Rc::clone(&closure.env), params.clone(), args)?
            } else {
                return Err(ScmErr::Arity("closure".to_owned(), 2));
            }
        }
        Formals::Rest(params, symbol) => {
            let mut full_params = params.clone();
            full_params.push(symbol);

            let mut full_args: Vec<ScmVal> = args[..params.len()].into();
            let rest = ScmVal::vec_to_list(args[params.len()..].into(), ScmVal::Empty);
            full_args.push(rest);

            Env::bind_in_new_env(Rc::clone(&closure.env), full_params, full_args)?
        }
    };

    // Eval all body exressions but the last one
    for expr in &closure.body[..closure.body.len() - 1] {
        eval_tco(expr.clone(), Rc::clone(&bound_env), false)?;
    }

    // Return the tail expression unevaluated and the updated capture environment for tco
    Ok((closure.body[closure.body.len() - 1].clone(), bound_env))
}

// Apply a function to a list of values.
// Only works right now if the second arg is a list and ignores additional args.
// Not sure if this is the intended r5rs behaviour.
pub fn user_apply(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 2 {
        let func = args[0].clone();
        let list = match args[1].clone() {
            ScmVal::Pair(p) => ScmVal::Pair(p),
            e => return Err(ScmErr::BadArgType("apply".to_owned(), "pair".to_owned(), e)),
        };
        Ok(ScmVal::cons(func, list))
    } else {
        Err(ScmErr::Arity("apply".to_owned(), 2))
    }
}

// Evaluates the first argument using a user supplied environment.
pub fn user_eval(args: Vec<ScmVal>) -> TcoResult {
    if args.len() >= 2 {
        let expr = args[0].clone();
        match args[1].clone() {
            ScmVal::Env(env) => Ok((expr, env)),
            e => Err(ScmErr::BadArgType(
                "eval".to_owned(),
                "environment".to_owned(),
                e,
            )),
        }
    } else {
        Err(ScmErr::Arity("apply".to_owned(), 2))
    }
}

// Derived Expressions ////////////////////////////////////////////////////////

pub fn eval_and(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() == 0 {
        return Ok(ScmVal::Boolean(true));
    }

    // if all values are true before the last one the last one is tco returned
    for arg in args[..args.len() - 1].iter() {
        let result = eval_tco(arg.clone(), Rc::clone(&env), false)?;
        if !proc::is_true(result) {
            return Ok(ScmVal::Boolean(false));
        }
    }
    Ok(args[args.len() - 1].clone())
}

pub fn eval_or(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> (ValResult, bool) {
    if args.len() == 0 {
        return (Ok(ScmVal::Boolean(false)), false);
    }

    // if all values are true before the last one the last one is tco returned
    for arg in args[..args.len() - 1].iter() {
        let result = eval_tco(arg.clone(), Rc::clone(&env), false);
        let result = match result {
            Ok(res) => res,
            Err(_) => return (result, false),
        };

        if proc::is_true(result.clone()) {
            // This is a problem because result gets evalled again
            return (Ok(result), false);
        }
    }
    (Ok(args[args.len() - 1].clone()), true)
}

pub fn eval_begin(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() == 0 {
        return Ok(ScmVal::Empty);
    }

    for expr in args[..args.len() - 1].iter() {
        eval_tco(expr.clone(), Rc::clone(&env), false)?;
    }

    Ok(args[args.len() - 1].clone())
}

// Eval a do statment by restructuring it into a letrec and evaluating it.
// Would benefit from a nice macro.
pub fn eval_do(args: Vec<ScmVal>) -> ValResult {
    // (do ((var init step) ...)
    //    (cond result)
    //  commands ...)
    //  =>
    //  (letrec ((loop (lambda (var ...)
    //                   (if cond
    //                       result
    //                       (begin commands ...
    //                              (loop step ...))))))
    //    (loop init)))
    if args.len() < 2 {
        return Err(ScmErr::Arity("do".to_owned(), 2));
    }

    // Get lists of params, their initial values, and their step expressions
    let (vars, inits, steps) = unbind_do(args[0].clone())?;

    // Get the condition and result separated
    let test_vec = ScmVal::list_to_vec(args[1].clone()).ok_or(ScmErr::BadArgType(
        "do test".to_owned(),
        "pair".to_owned(),
        args[1].clone(),
    ))?;
    if test_vec.len() < 1 {
        return Err(ScmErr::Arity("do test".to_owned(), 1));
    }
    let cond = test_vec[0].clone();
    let result = if test_vec.len() > 1 {
        test_vec[1].clone()
    } else {
        ScmVal::Empty
    };

    // Build the false branch with begin, commands, and recursive call to loop
    let loop_rec = ScmVal::cons(ScmVal::new_sym("loop"), steps);
    let mut commands: Vec<ScmVal> = args[2..].into();
    commands.push(loop_rec);
    let begin = ScmVal::cons(
        ScmVal::new_sym("begin"),
        ScmVal::vec_to_list(commands, ScmVal::Empty),
    );

    // Build the if statement with the condition, result, and begin branch
    let if_stmt = ScmVal::vec_to_list(
        vec![ScmVal::new_sym("if"), cond, result, begin],
        ScmVal::Empty,
    );

    // Build the the loop binding
    let lambda = ScmVal::vec_to_list(
        vec![ScmVal::new_sym("lambda"), vars, if_stmt],
        ScmVal::Empty,
    );
    let loop_bind = ScmVal::vec_to_list(vec![ScmVal::new_sym("loop"), lambda], ScmVal::Empty);
    let bindings = ScmVal::cons(loop_bind, ScmVal::Empty);

    // Build the letrec
    let call = ScmVal::cons(ScmVal::new_sym("loop"), inits);
    let letrec = ScmVal::vec_to_list(
        vec![ScmVal::new_sym("letrec"), bindings, call],
        ScmVal::Empty,
    );

    // Return the new expression to be evaluated
    Ok(letrec)
}

// cond
// case
// named let
// quasiquote(`)

// Helpers ////////////////////////////////////////////////////////////////////

fn eval_vec(v: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ScmResult<Vec<ScmVal>> {
    let evalled: Result<Vec<ScmVal>, _> = v
        .into_iter()
        .map(|val| eval_tco(val, Rc::clone(&env), false))
        .collect();
    Ok(evalled?)
}

fn unbind(list: ScmVal) -> ScmResult<(Vec<ScmVal>, Vec<ScmVal>)> {
    // Get a vector of binding lists. i.e. ((sym pair) ...) => [(sym pair) ...]
    let vec = ScmVal::list_to_vec(list.clone()).ok_or(ScmErr::BadArgType(
        "let/letrec".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(sym pair) ...] => [(sym, pair) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let binding = ScmVal::list_to_vec(p.clone()).ok_or(ScmErr::BadArgType(
                "let/letrec bindings".to_owned(),
                "pair".to_owned(),
                p.clone(),
            ))?;

            // Ensure there are two elements in the binding
            if binding.len() >= 2 {
                Ok((binding[0].clone(), binding[1].clone()))
            } else {
                Err(ScmErr::BadArgType(
                    "let/letrec bindings".to_owned(),
                    "(sym pair)".to_owned(),
                    p,
                ))
            }
        })
        .collect();

    // return (param_vec, args_vec)
    Ok(bindings_vec?.into_iter().unzip())
}

fn letrec_bind(list: ScmVal) -> ScmResult<(Vec<ScmVal>, Vec<ScmVal>)> {
    let (params, args) = unbind(list)?;

    // Create let bindings to declare the vars ((val1 <undefined>) ...)
    let bindings = params
        .clone()
        .into_iter()
        .map(|p| ScmVal::cons(p, ScmVal::Undefined))
        .collect();

    // Create the assingments to add to the start of the body
    // ((set! val1 arg1) ...)
    let assignments = zip(params.clone(), args.clone())
        .map(|(p, a)| ScmVal::vec_to_list(vec![ScmVal::new_sym("set!"), p, a], ScmVal::Empty))
        .collect();

    Ok((bindings, assignments))
}

fn unbind_do(list: ScmVal) -> ScmResult<(ScmVal, ScmVal, ScmVal)> {
    // Get a vector of binding lists. i.e. ((var init step) ...) => [(var init step) ...]
    let vec = ScmVal::list_to_vec(list.clone()).ok_or(ScmErr::BadArgType(
        "do".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(var init step) ...] => [(var init step) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let binding = ScmVal::list_to_vec(p.clone()).ok_or(ScmErr::BadArgType(
                "do bindings".to_owned(),
                "pair".to_owned(),
                p.clone(),
            ))?;

            // Ensure there are at least two elements in the binding
            match binding.len() {
                3.. => Ok((binding[0].clone(), binding[1].clone(), binding[2].clone())),
                2 => Ok((binding[0].clone(), binding[1].clone(), binding[0].clone())),
                _ => Err(ScmErr::BadArgType(
                    "let/letrec bindings".to_owned(),
                    "(sym pair)".to_owned(),
                    p,
                )),
            }
        })
        .collect();

    let mut vars_vec = Vec::new();
    let mut inits_vec = Vec::new();
    let mut steps_vec = Vec::new();
    for bind in bindings_vec?.iter() {
        vars_vec.push(bind.0.clone());
        inits_vec.push(bind.1.clone());
        steps_vec.push(bind.2.clone());
    }

    // return (vars, inits, steps)
    Ok((
        ScmVal::vec_to_list(vars_vec, ScmVal::Empty),
        ScmVal::vec_to_list(inits_vec, ScmVal::Empty),
        ScmVal::vec_to_list(steps_vec, ScmVal::Empty),
    ))
}
