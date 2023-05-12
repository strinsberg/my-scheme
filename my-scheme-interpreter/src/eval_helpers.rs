use crate::builtin::Builtin;
use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Closure, ConsCell, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

// Eval Helpers and Transformations ///////////////////////////////////////////

// Takes the arguments to a lambda expression and the current env and creates
// a new closure.
pub fn make_closure(args: Vec<ScmVal>, env: Rc<RefCell<Env>>) -> ValResult {
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
