use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Closure, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

// Eval Helpers and Transformations ///////////////////////////////////////////

// Takes the arguments to a lambda expression and the current env and creates
// a new closure.
pub fn make_closure(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
    if args.len() >= 2 {
        let params = args[0].clone();
        let formals = match params {
            ScmVal::NewSymbol(_) => Formals::Collect(params),
            ScmVal::NewPair(_) => {
                let (vec, dotted) = ScmVal::list_to_vec(&params).ok_or(ScmErr::BadArgType(
                    "lambda".to_owned(),
                    "pair".to_owned(),
                    args[0].clone(),
                ))?;
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

// Transforms a let expression into a lambda application.
// Example: (let ((a 1) (b 2)) (+ a b)) => ((lambda (a b) (+ a b)) 1 2)
pub fn transform_let(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
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

// Transforms a let* expression into a let expression.
// Example: (let* ((a 1) (b 2)) (+ a b)) => (let ((a 1)) (let* ((b 2)) (+ a b)))
//
// TODO this feels horribly inefficient with all the to and from vecs, and I don't
// really like transforming it into something that will have to be transformed
// again and again into so many nested lets/closures.
pub fn transform_let_star(args: Vec<ScmVal>) -> ValResult {
    if args.len() >= 2 {
        let (bindings, dot) =
            ScmVal::list_to_vec(&args[0]).ok_or(ScmErr::Syntax(args[0].clone()))?;
        if dot {
            return Err(ScmErr::Syntax(args[0].clone()));
        }

        let num_bind = bindings.len();
        match num_bind {
            2.. => {
                let let_bind = ScmVal::new_pair(bindings[0].clone(), ScmVal::Empty);
                let body = ScmVal::vec_to_list(args[1..].into(), ScmVal::Empty);
                let let_star_bind = ScmVal::vec_to_list(bindings[1..].into(), ScmVal::Empty);
                let let_star =
                    ScmVal::vec_to_list(vec![ScmVal::new_sym("let*"), let_star_bind], body);
                println!("{let_bind}, {let_star}");
                Ok(ScmVal::vec_to_list(
                    vec![ScmVal::new_sym("let"), let_bind, let_star],
                    ScmVal::Empty,
                ))
            }
            1 => {
                let let_bind = ScmVal::new_pair(bindings[0].clone(), ScmVal::Empty);
                let body = ScmVal::vec_to_list(args[1..].into(), ScmVal::Empty);
                Ok(ScmVal::vec_to_list(
                    vec![ScmVal::new_sym("let"), let_bind],
                    body,
                ))
            }
            _ => Ok(ScmVal::new_pair(
                ScmVal::new_sym("begin"),
                ScmVal::vec_to_list(args[1..].into(), ScmVal::Empty),
            )),
        }
    } else {
        Err(ScmErr::Arity("let".to_owned(), 2))
    }
}

// Transforms a letrec expression into a let expression.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn transform_letrec(args: Vec<ScmVal>) -> ValResult {
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
        Err(ScmErr::Arity("letrec".to_owned(), 2))
    }
}

// Eval a do statment by restructuring it into a letrec and evaluating it.
// Would benefit from a nice macro.
pub fn transform_do(args: Vec<ScmVal>) -> ValResult {
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
    let (test_vec, _) = ScmVal::list_to_vec(&args[1]).ok_or(ScmErr::BadArgType(
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

// Binding Helpers ////////////////////////////////////////////////////////////

pub fn bind_closure_args(closure: Rc<Closure>, args: Vec<ScmVal>) -> ScmResult<Rc<RefCell<Env>>> {
    match closure.params.clone() {
        Formals::Collect(symbol) => Ok(Env::bind_in_new_env(
            Rc::clone(&closure.env),
            vec![symbol],
            vec![ScmVal::vec_to_list_mut(args, ScmVal::Empty)],
        )?),
        Formals::Fixed(params) => {
            if args.len() >= params.len() {
                Ok(Env::bind_in_new_env(
                    Rc::clone(&closure.env),
                    params.clone(),
                    args,
                )?)
            } else {
                return Err(ScmErr::Arity("closure".to_owned(), params.len()));
            }
        }
        Formals::Rest(params, symbol) => {
            let mut full_params = params.clone();
            full_params.push(symbol);

            let mut full_args: Vec<ScmVal> = args[..params.len()].into();
            let rest = ScmVal::vec_to_list_mut(args[params.len()..].into(), ScmVal::Empty);
            full_args.push(rest);

            Ok(Env::bind_in_new_env(
                Rc::clone(&closure.env),
                full_params,
                full_args,
            )?)
        }
    }
}

fn unbind(list: ScmVal) -> ScmResult<(Vec<ScmVal>, Vec<ScmVal>)> {
    // Get a vector of binding lists. i.e. ((sym pair) ...) => [(sym pair) ...]
    let (vec, _) = ScmVal::list_to_vec(&list).ok_or(ScmErr::BadArgType(
        "let/letrec".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(sym pair) ...] => [(sym, pair) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let (binding, _) = ScmVal::list_to_vec(&p).ok_or(ScmErr::BadArgType(
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
    let (vec, _) = ScmVal::list_to_vec(&list).ok_or(ScmErr::BadArgType(
        "do".to_owned(),
        "pair".to_owned(),
        list.clone(),
    ))?;

    // Convert the pairs into rust pairs. i.e. [(var init step) ...] => [(var init step) ...]
    let bindings_vec: ScmResult<Vec<(ScmVal, ScmVal, ScmVal)>> = vec
        .into_iter()
        .map(|p| {
            let (binding, _) = ScmVal::list_to_vec(&p).ok_or(ScmErr::BadArgType(
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
