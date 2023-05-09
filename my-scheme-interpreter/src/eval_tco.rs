use crate::builtin::Builtin;
use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Closure, ConsCell, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

// TODO named let
// TODO quasiquote(`)
// TODO delay and force
// TODO begin has two forms, one that is at the top level and is allowed to have
// defines inside it (I think). Currently, that will cause an error.
//
// TODO Macros -- note that I am not sure when macros should be expanded. It is
// relatively clear that they must be done at eval time since they are themselves
// evaluated, but perhaps top level expressions could be traversed once to expand
// any macros before evaluating the expression. This would keep macros from
// being present in a lambda body and having to be transformed every single call.
// Though perhaps this approach could be used when evaluating a lambda or any
// structure that might save code to be called later. Just macro expand the body
// when saving it. Then each call would use the full code. Probably macro expanding
// has to be recursive until there are no more macros to expand.
//
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

// Flag enum to tell eval_tco if it needs to evaluate an expression again or
// to return it. This makes it more readable than passing true and false around.
enum Tco {
    Yes,
    No,
}

// Evaluates a vector of forms and returns the value of the last one.
pub fn eval_forms(forms: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if forms.len() < 1 {
        return Ok(ScmVal::Empty);
    }

    for f in forms[..forms.len() - 1].iter() {
        eval_top_level(f.clone(), Rc::clone(&env))?;
    }
    eval_top_level(forms[forms.len() - 1].clone(), env)
}

// Evaluate a form that is not at the top level of a file or repl
pub fn eval(value: ScmVal, environment: Rc<RefCell<Env>>) -> ValResult {
    eval_tco(value, environment, false)
}

// Evalate a form that is at the top level of a file or repl
pub fn eval_top_level(value: ScmVal, environment: Rc<RefCell<Env>>) -> ValResult {
    eval_tco(value, environment, true)
}

// Evaluate a scheme expression with tail call optimisation. The flag is_top_level
// is used to prevent define being used inside other forms.
fn eval_tco(value: ScmVal, environment: Rc<RefCell<Env>>, is_top_level: bool) -> ValResult {
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
            // Cannot evaluate a vector
            ScmVal::Vector(_) | ScmVal::VectorMut(_) => Err(ScmErr::Syntax(expr)),
            // Lists have their first arg applied to the cdr
            ScmVal::PairMut(cell) => {
                let mut _do_tco = Tco::No;
                let c = (*cell.borrow()).clone();
                (expr, env, _do_tco) = eval_pair(c, expr.clone(), Rc::clone(&env), is_top_level)?;
                match _do_tco {
                    Tco::Yes => continue,
                    Tco::No => return Ok(expr),
                }
            }
            ScmVal::Pair(cell) => {
                let mut _do_tco = Tco::No;
                let c = (*cell).clone();
                (expr, env, _do_tco) = eval_pair(c, expr.clone(), Rc::clone(&env), is_top_level)?;
                match _do_tco {
                    Tco::Yes => continue,
                    Tco::No => return Ok(expr),
                }
            }
            // String, Bool, Char, Closure, Procedure all eval to themselves
            _ => Ok(expr),
        };
    }
}

// This is a complex process of deciding what the first element of the list
// that is being called as a function application. We return an expression, environment,
// and flag. The flag tells the eval_tco function whether or not the expression needs
// to be returned or evaluated with the new environment.
fn eval_pair(
    cell: ConsCell,
    expr: ScmVal,
    env: Rc<RefCell<Env>>,
    is_top_level: bool,
) -> ScmResult<(ScmVal, Rc<RefCell<Env>>, Tco)> {
    match cell.head.clone() {
        // Symbols need to be applied as their bound values or as keywords/derived exprs
        ScmVal::Symbol(name) => eval_pair_with_symbol_head(
            &name.to_string(),
            cell.head.clone(),
            cell.tail.clone(),
            is_top_level,
            Rc::clone(&env),
        ),
        // Closures are applied to their evaluated arguments. NEEDS tco.
        ScmVal::Closure(c) => {
            let args = eval_args(cell.tail, expr, Rc::clone(&env))?;
            let (expr, new_env) = apply_closure(c, args)?;
            Ok((expr, new_env, Tco::Yes))
        }
        // Core procs check for eval or apply first an then apply the proc to evaluated arguments
        ScmVal::Core(b) => {
            let args = eval_args(cell.tail, expr, Rc::clone(&env))?;
            match b {
                // These NEED their results evaluated with TCO
                Builtin::Apply => Ok((user_apply(args)?, Rc::clone(&env), Tco::Yes)),
                Builtin::Eval => {
                    let (expr, new_env) = user_eval(args)?;
                    Ok((expr, new_env, Tco::Yes))
                }
                // Core procs do not need tco
                _ => Ok((proc::apply_core_proc(b, args)?, Rc::clone(&env), Tco::No)),
            }
        }
        // If the head is a pair evaluate it and replace the head and evaluate
        // the expression again. NEEDS tco.
        ScmVal::Pair(_) | ScmVal::PairMut(_) => {
            let proc = eval(cell.head.clone(), Rc::clone(&env))?;
            Ok((ScmVal::cons(proc, cell.tail), Rc::clone(&env), Tco::Yes))
        }
        _ => Err(ScmErr::Syntax(expr)),
    }
}

// Simple helper to turn an arguments list into a vector and evaluate it.
fn eval_args(args: ScmVal, expr: ScmVal, env: Rc<RefCell<Env>>) -> ScmResult<Vec<ScmVal>> {
    let (args_vec, dotted, _) = ScmVal::list_to_vec(args).ok_or(ScmErr::Syntax(expr.clone()))?;
    if dotted {
        return Err(ScmErr::Syntax(expr));
    }
    eval_vec(args_vec, Rc::clone(&env))
}

// Evaluated a pair/list where the first element is a symbol. We either look up
// the symbol and apply it's result or use the name as a keyword to apply the
// correct procedure or derived expression.
fn eval_pair_with_symbol_head(
    name: &str,
    head: ScmVal,
    tail: ScmVal,
    is_top_level: bool,
    env: Rc<RefCell<Env>>,
) -> ScmResult<(ScmVal, Rc<RefCell<Env>>, Tco)> {
    // To make helper calls and returns cleaner by reducing Rc::clone(&env) calls and 3 tuples
    let clone_env = Rc::clone(&env);
    let do_tco = |val| Ok((val, Rc::clone(&clone_env), Tco::Yes));
    let dont_tco = |val| Ok((val, Rc::clone(&clone_env), Tco::No));

    // Check if the symbol has been bound and evaluate accordingly
    let lookup = clone_env.borrow().lookup(head.clone());
    match lookup {
        Some(proc) => do_tco(ScmVal::cons(proc, tail.clone())),
        None => {
            // create an args vector and error if the list is improper
            let (args, dotted, _) = ScmVal::list_to_vec(tail.clone())
                .ok_or(ScmErr::Syntax(ScmVal::cons(head.clone(), tail.clone())))?;
            if dotted {
                return Err(ScmErr::Syntax(ScmVal::cons(head.clone(), tail.clone())));
            }

            match name {
                // These do not need their results evaluated with tco
                "quote" => dont_tco(args[0].clone()),
                "lambda" => dont_tco(eval_lambda(args, env)?),
                "set!" => dont_tco(eval_set(args, env)?),
                "define" => {
                    if is_top_level {
                        dont_tco(eval_define(args, env)?)
                    } else {
                        Err(ScmErr::InnerDefine)
                    }
                }
                // These NEED their results evalated with tco
                "if" => do_tco(eval_if(args, env)?),
                "let" => do_tco(eval_let(args, env)?),
                "let*" => do_tco(eval_let_star(args, env)?),
                "letrec" => do_tco(eval_letrec(args)?),
                "and" => do_tco(eval_and(args, env)?),
                "or" => {
                    let (result, do_tco) = eval_or(args, Rc::clone(&env));
                    Ok((result?, env, do_tco))
                }
                "do" => do_tco(eval_do(args)?),
                "begin" => do_tco(eval_begin(args, env)?),
                "cond" => do_tco(eval_cond(args, env)?),
                "case" => do_tco(eval_case(args, env)?),
                // Add aditional derived expressions here

                // Undeclared name
                _ => Err(ScmErr::Undeclared(name.to_string())),
            }
        }
    }
}

// Eval Core Forms ////////////////////////////////////////////////////////////

// Evaluates an if statement with tco.
// Returns the unevaluated expression for the true or false branch based on the
// evaluation of the condition expression.
// Any branch past the false branch is ignored.
fn eval_if(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
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
    let result = eval(args[0].clone(), env)?;
    // return the correct branch unevaluated
    if !proc::is_true(result) {
        if false_branch {
            Ok(args[2].clone())
        } else {
            Ok(ScmVal::Empty)
        }
    } else {
        Ok(args[1].clone())
    }
}

// Evaluates a lambda statment into a closure and returns it.
fn eval_lambda(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let params = args[0].clone();
        let formals =
            match params {
                ScmVal::Symbol(_) => Formals::Collect(params),
                ScmVal::Pair(_) | ScmVal::PairMut(_) => {
                    let (vec, dotted, _) = ScmVal::list_to_vec(params).ok_or(
                        ScmErr::BadArgType("lambda".to_owned(), "pair".to_owned(), args[0].clone()),
                    )?;
                    if dotted {
                        Formals::Rest(vec[..vec.len() - 1].into(), vec[vec.len() - 1].clone())
                    } else {
                        Formals::Fixed(vec)
                    }
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
            "no-name",
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
fn eval_let(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let bindings = args[0].clone();
        let (params, bind_args) = unbind(bindings.clone())?;
        Ok(ScmVal::cons(
            ScmVal::new_closure(Closure::new(
                "no-name",
                env,
                Formals::Fixed(params),
                args[1..].into(),
            )),
            ScmVal::vec_to_list(bind_args, ScmVal::Empty),
        ))
    } else {
        Err(ScmErr::Arity("let".to_owned(), 2))
    }
}

// It is easier not to convert this one into a million nested let expressions
// but instead look through the bindings and add them to the env one at a time
// before evaluating the body.
fn eval_let_star(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        // Get bindings list as a vector
        let (bindings_vec, _, _) = ScmVal::list_to_vec(args[0].clone()).ok_or(
            ScmErr::BadArgType("let*".to_owned(), "pair".to_owned(), args[0].clone()),
        )?;

        // Add a scope to the passed env to get a new env
        let new_env = Env::add_scope(env);

        // loop through the bindings and bind their evaluated value in the new env
        // sequentially. This makes it possible for a binding to reference previous
        // bindings, but does not allow for mutal recursion like letrec.
        for bind in bindings_vec.iter() {
            let (bind_vec, _, _) = ScmVal::list_to_vec(bind.clone()).ok_or(ScmErr::BadArgType(
                "let* binding".to_owned(),
                "pair".to_owned(),
                args[0].clone(),
            ))?;
            if bind_vec.len() < 2 {
                return Err(ScmErr::Arity("let* binding".to_owned(), 2));
            }

            let value = eval(bind_vec[1].clone(), Rc::clone(&new_env))?;
            new_env.borrow_mut().insert(bind_vec[0].clone(), value)?;
        }

        // Put the new env into a closure that takes no arguments and call it with
        // not arguments. I.e. ((lambda () body))
        Ok(ScmVal::new_pair(
            ScmVal::new_closure(Closure::new(
                "no-name",
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
fn eval_letrec(args: Vec<ScmVal>) -> ValResult {
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
fn eval_set(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let key = args[0].clone();
        let val = match eval(args[1].clone(), Rc::clone(&env))? {
            ScmVal::Closure(c) => ScmVal::new_closure(Closure::new(
                &key.to_string(),
                Rc::clone(&c.env),
                c.params.clone(),
                c.body.clone(),
            )),
            v => v,
        };
        env.borrow_mut().set(key.clone(), val.clone())
    } else {
        Err(ScmErr::Arity("set!".to_owned(), 2))
    }
}

// Evaluate a define statement by first setting the variable to <undefined>
// and then using set! to store the new evaluated value.
fn eval_define(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("define".to_owned(), 2));
    }

    let first = args[0].clone();
    match first {
        ScmVal::Symbol(_) => {
            env.borrow_mut().insert(first.clone(), ScmVal::Undefined)?;
            eval_set(args, env)
        }
        ScmVal::Pair(cell) => {
            // just rebuild with (define name lambda) and re-evaluate
            let name = cell.head.clone();
            let params = cell.tail.clone();
            let body = ScmVal::vec_to_list(args[1..].into(), ScmVal::Empty);
            let lambda = ScmVal::cons(ScmVal::new_sym("lambda"), ScmVal::cons(params, body));
            eval_top_level(
                ScmVal::vec_to_list(vec![ScmVal::new_sym("define"), name, lambda], ScmVal::Empty),
                env,
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
fn apply_closure(closure: Rc<Closure>, args: Vec<ScmVal>) -> ScmResult<(ScmVal, Rc<RefCell<Env>>)> {
    // Bind the arguments to their parameters according to the formals list
    let bound_env = match closure.params.clone() {
        Formals::Collect(symbol) => Env::bind_in_new_env(
            Rc::clone(&closure.env),
            vec![symbol],
            vec![ScmVal::vec_to_list_mut(args, ScmVal::Empty)],
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
            let rest = ScmVal::vec_to_list_mut(args[params.len()..].into(), ScmVal::Empty);
            full_args.push(rest);

            Env::bind_in_new_env(Rc::clone(&closure.env), full_params, full_args)?
        }
    };

    // Eval all body exressions but the last one
    for expr in &closure.body[..closure.body.len() - 1] {
        eval(expr.clone(), Rc::clone(&bound_env))?;
    }

    // Return the tail expression unevaluated and the updated capture environment for tco
    Ok((closure.body[closure.body.len() - 1].clone(), bound_env))
}

// Apply a function to a list of values.
// Only works right now if the second arg is a list and ignores additional args.
// Not sure if this is the intended r5rs behaviour.
fn user_apply(args: Vec<ScmVal>) -> ValResult {
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
fn user_eval(args: Vec<ScmVal>) -> ScmResult<(ScmVal, Rc<RefCell<Env>>)> {
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

fn eval_and(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() == 0 {
        return Ok(ScmVal::Boolean(true));
    }

    // if all values are true before the last one the last one is tco returned
    for arg in args[..args.len() - 1].iter() {
        let result = eval(arg.clone(), Rc::clone(&env))?;
        if !proc::is_true(result) {
            return Ok(ScmVal::Boolean(false));
        }
    }
    Ok(args[args.len() - 1].clone())
}

fn eval_or(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> (ValResult, Tco) {
    if args.len() == 0 {
        return (Ok(ScmVal::Boolean(false)), Tco::No);
    }

    // if all values are true before the last one the last one is tco returned
    for arg in args[..args.len() - 1].iter() {
        let result = eval(arg.clone(), Rc::clone(&env));
        let result = match result {
            Ok(res) => res,
            Err(_) => return (result, Tco::No),
        };

        if proc::is_true(result.clone()) {
            return (Ok(result), Tco::No);
        }
    }
    (Ok(args[args.len() - 1].clone()), Tco::Yes)
}

fn eval_begin(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() == 0 {
        return Ok(ScmVal::Empty);
    }

    for expr in args[..args.len() - 1].iter() {
        eval(expr.clone(), Rc::clone(&env))?;
    }

    Ok(args[args.len() - 1].clone())
}

// Eval a do statment by restructuring it into a letrec and evaluating it.
// Would benefit from a nice macro.
fn eval_do(args: Vec<ScmVal>) -> ValResult {
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
    let (test_vec, _, _) = ScmVal::list_to_vec(args[1].clone()).ok_or(ScmErr::BadArgType(
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

fn eval_cond(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() < 1 {
        return Err(ScmErr::Arity("cond".to_owned(), 1));
    }

    for arg in args.iter() {
        match arg {
            ScmVal::Pair(_) | ScmVal::PairMut(_) => {
                let (vec, dotted, _) = ScmVal::list_to_vec(arg.clone()).unwrap();
                if dotted {
                    if vec.len() >= 2 && vec[1] == ScmVal::new_sym("=>") {
                        return Err(ScmErr::Syntax(arg.clone()));
                    } else {
                        match eval_cond_condition(vec, Rc::clone(&env)) {
                            Some(result_expr) => return result_expr,
                            None => continue,
                        }
                    }
                } else {
                    match eval_cond_condition(vec, Rc::clone(&env)) {
                        Some(result_expr) => return result_expr,
                        None => continue,
                    }
                }
            }
            _ => {
                return Err(ScmErr::BadArgType(
                    "cond".to_owned(),
                    "pair or dotted pair".to_owned(),
                    arg.clone(),
                ))
            }
        }
    }
    Ok(ScmVal::Empty)
}

fn eval_cond_condition(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> Option<ValResult> {
    if args.len() < 2 {
        return Some(Err(ScmErr::Arity("cond condition".to_owned(), 2)));
    } else if args[0].clone() == ScmVal::new_sym("else") {
        // if this is an else branch it is automatically true
        return Some(eval_begin(args[1..].into(), env));
    }

    // Evaluate the test
    let test = match eval(args[0].clone(), Rc::clone(&env)) {
        Ok(val) => val,
        Err(e) => return Some(Err(e)),
    };

    // If the test is true we evaluate the body
    if proc::is_true(test.clone()) {
        // pass test as the first argument to procedure
        if args[1].clone() == ScmVal::new_sym("=>") {
            if args.len() < 3 {
                return Some(Err(ScmErr::Arity("cond arrow(=>) condition".to_owned(), 2)));
            }
            // Could eval args[2] to see if it is a valid procedure but to keep it simple
            // we just pass (proc (quote test)) to be evaluated. It will error if it is a bad
            // function call.
            Some(Ok(ScmVal::vec_to_list(
                vec![
                    args[2].clone(),
                    ScmVal::vec_to_list(vec![ScmVal::new_sym("quote"), test], ScmVal::Empty),
                ],
                ScmVal::Empty,
            )))
        } else {
            return Some(eval_begin(args[1..].into(), env));
        }
    } else {
        None
    }
}

fn eval_case(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() < 2 {
        return Err(ScmErr::Arity("case".to_owned(), 2));
    }

    let test = eval(args[0].clone(), Rc::clone(&env))?;

    for arg in args[1..].iter() {
        match arg {
            ScmVal::Pair(_) | ScmVal::PairMut(_) => {
                let (vec, _, _) = ScmVal::list_to_vec(arg.clone()).unwrap();
                match eval_case_condition(test.clone(), vec, Rc::clone(&env)) {
                    Some(result_expr) => return result_expr,
                    None => continue,
                }
            }
            _ => {
                return Err(ScmErr::BadArgType(
                    "case".to_owned(),
                    "pair".to_owned(),
                    arg.clone(),
                ))
            }
        }
    }
    Ok(ScmVal::Empty)
}

fn eval_case_condition(
    case: ScmVal,
    args: Vec<ScmVal>,
    env: Rc<RefCell<Env>>,
) -> Option<ValResult> {
    if args.len() < 2 {
        return Some(Err(ScmErr::Arity("case condition".to_owned(), 2)));
    } else if args[0].clone() == ScmVal::new_sym("else") {
        // if this is an else branch it is automatically true
        return Some(eval_begin(args[1..].into(), env));
    }

    // Test the results agains all datum
    let (datum, _, _) = match ScmVal::list_to_vec(args[0].clone()) {
        Some(d) => d,
        None => {
            return Some(Err(ScmErr::BadArgType(
                "case".to_owned(),
                "pair of datum".to_owned(),
                args[0].clone(),
            )))
        }
    };

    // If any of the datum match the case then eval the expressions
    for d in datum.iter() {
        if proc::eqv(d.clone(), case.clone()) {
            return Some(eval_begin(args[1..].into(), env));
        }
    }

    None
}

// Helpers ////////////////////////////////////////////////////////////////////

fn eval_vec(v: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ScmResult<Vec<ScmVal>> {
    let evalled: Result<Vec<ScmVal>, _> = v
        .into_iter()
        .map(|val| eval(val, Rc::clone(&env)))
        .collect();
    Ok(evalled?)
}

fn unbind(list: ScmVal) -> ScmResult<(Vec<ScmVal>, Vec<ScmVal>)> {
    // Get a vector of binding lists. i.e. ((sym pair) ...) => [(sym pair) ...]
    let (vec, _, _) = ScmVal::list_to_vec(list.clone()).ok_or(ScmErr::BadArgType(
        "let/letrec".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(sym pair) ...] => [(sym, pair) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let (binding, _, _) = ScmVal::list_to_vec(p.clone()).ok_or(ScmErr::BadArgType(
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
    let (vec, _, _) = ScmVal::list_to_vec(list.clone()).ok_or(ScmErr::BadArgType(
        "do".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(var init step) ...] => [(var init step) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let (binding, _, _) = ScmVal::list_to_vec(p.clone()).ok_or(ScmErr::BadArgType(
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
