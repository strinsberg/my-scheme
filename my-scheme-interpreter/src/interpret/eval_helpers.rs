use crate::data::cell::Cell;
use crate::data::env::Env;
use crate::data::err::Error;
use crate::data::proc::{Closure, Formals};
use crate::data::string::Str;
use crate::data::value::Value;
use crate::proc::utils;
use std::rc::Rc;

// Eval Helpers and Transformations ///////////////////////////////////////////

// Takes the arguments to a lambda expression and the current env and creates
// a new closure.
pub fn make_closure(args: &Value, env: Rc<Env<Str, Value>>) -> Result<Value, Error> {
    let (params, body) = utils::rest_take_1(args)?;
    let formals = match params {
        Value::Symbol(s) => Formals::Collect(s),
        Value::Pair(cell) => formals_from_cell(cell)?,
        Value::Empty => Formals::Fixed(vec![]),
        _ => return Err(Error::BadArg(1)),
    };

    Ok(Value::from(Closure::new(None, env, formals, body)))
}

fn formals_from_cell(cell: Rc<Cell<Value>>) -> Result<Formals, Error> {
    println!("formals -- {}", cell);
    let mut vec = Vec::new();
    for c in cell.cells() {
        match c.head().clone() {
            Value::Symbol(s) => vec.push(s),
            _ => return Err(Error::BadArg(1)),
        }

        if c.is_dotted() {
            match c
                .tail()
                .as_ref()
                .expect("dotted cell tail should not be None")
                .clone()
            {
                Value::Symbol(s) => {
                    return Ok(Formals::Rest(vec, s));
                }
                _ => return Err(Error::BadArg(1)),
            }
        }
    }
    println!("{:?}", vec);
    Ok(Formals::Fixed(vec))
}

// Transforms a let expression into a lambda application.
// Example: (let ((a 1) (b 2)) (+ a b)) => ((lambda (a b) (+ a b)) 1 2)
pub fn transform_let(args: &Value, env: Rc<Env<Str, Value>>) -> Result<Value, Error> {
    let (bindings, body) = utils::rest_take_1(args)?;
    let (params, bind_args) = unbind(bindings.clone())?;
    Ok(Value::from(Cell::new(
        Value::from(Closure::new(None, env, Formals::Fixed(params), body)),
        Some(Value::list_from_vec(bind_args, Value::Empty)),
    )))
}

// Transforms a let* expression into a let expression.
// Example: (let* ((a 1) (b 2)) (+ a b)) => (let ((a 1)) (let* ((b 2)) (+ a b)))
//
// TODO this feels horribly inefficient with all the to and from vecs, and I don't
// really like transforming it into something that will have to be transformed
// again and again into so many nested lets/closures.
pub fn transform_let_star(args: &Value) -> Result<Value, Error> {
    let (bindings, body) = utils::rest_take_1(args)?;
    match utils::rest_take_1(&bindings) {
        Ok((bind, rest)) => {
            // bind is (sym pair), rest is ((sym pair)...)
            // want (let ((sym pair)) (let* ((sym pair) ...) body ...))
            let let_star = Value::list_from_vec(vec![Value::symbol(Str::from("let*")), rest], body);
            Ok(Value::list_from_vec(
                vec![
                    Value::symbol(Str::from("let")),
                    Value::from(Cell::new(bind, None)),
                    let_star,
                ],
                Value::Empty,
            ))
        }
        Err(_) => Ok(Value::from(Cell::new(
            Value::symbol(Str::from("begin")),
            Some(body),
        ))),
    }
}

// Transforms a letrec expression into a let expression.
// Example: (letrec ((a 1) (b 2)) (+ a b)) =>
//          (let ((a <undefined>) (b <undefined>))
//            (set! a 1) (set! b 2) (+ a b))
pub fn transform_letrec(args: &Value) -> Result<Value, Error> {
    let (bindings, body) = utils::rest_take_1(args)?;
    let (params, bind_args) = unbind(bindings.clone())?;
    let undef_binds = bind_undefined(&params);
    let new_body = letrec_body(&params, &bind_args, body);

    Ok(Value::list_from_vec(
        vec![Value::symbol(Str::from("let")), undef_binds],
        new_body,
    ))
}

fn bind_undefined(params: &Vec<Rc<Str>>) -> Value {
    let mut vec = Vec::new();
    for name in params.iter() {
        vec.push(Value::list_from_vec(
            vec![Value::Symbol(name.clone()), Value::Undefined],
            Value::Empty,
        ));
    }
    Value::list_from_vec(vec, Value::Empty)
}

fn letrec_body(params: &Vec<Rc<Str>>, bind_args: &Vec<Value>, body: Value) -> Value {
    let mut vec = Vec::new();
    for (i, name) in params.iter().enumerate() {
        vec.push(Value::list_from_vec(
            vec![
                Value::symbol(Str::from("set!")),
                Value::Symbol(name.clone()),
                bind_args[i].clone(),
            ],
            Value::Empty,
        ));
    }
    Value::list_from_vec(vec, body.clone())
}

// Eval a do statment by restructuring it into a letrec and evaluating it.
// Would benefit from a nice macro.
pub fn transform_do(args: &Value) -> Result<Value, Error> {
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

    // Destructure the do
    let (bindings, cond_expr, rest) = utils::rest_take_2(args)?;
    let (vars, inits, steps) = unbind_do(bindings.clone())?;
    let (cond, res) = utils::opt_last_take_2(&cond_expr).or(Err(Error::BadArg(2)))?;
    let result = match res {
        Some(val) => val,
        None => Value::Empty,
    };

    // Build the false branch with begin, commands, and recursive call to loop
    let loop_expr = Value::from(Cell::new(Value::symbol(Str::from("loop")), Some(steps)));
    let begin = match rest {
        Value::Empty => loop_expr,
        _ => {
            let mut result = Value::from(Cell::new(loop_expr, None));
            let cell = Value::get_pair_cell(&rest).expect("rest should be pair from rest_take_2");
            for val in cell.values() {
                result = Value::from(Cell::new(val, Some(result)));
            }
            Value::from(Cell::new(Value::symbol(Str::from("begin")), Some(result)))
        }
    };

    // Build the if expr with the condition, result, and begin branch
    let if_expr = Value::list_from_vec(
        vec![Value::symbol(Str::from("if")), cond, result, begin],
        Value::Empty,
    );

    // Build the the loop binding
    let lambda = Value::list_from_vec(
        vec![Value::symbol(Str::from("lambda")), vars, if_expr],
        Value::Empty,
    );
    let loop_bind =
        Value::list_from_vec(vec![Value::symbol(Str::from("loop")), lambda], Value::Empty);
    let bindings = Value::from(Cell::new(loop_bind, None));

    // Build the letrec
    let call = Value::from(Cell::new(Value::symbol(Str::from("loop")), Some(inits)));
    let letrec = Value::list_from_vec(
        vec![Value::symbol(Str::from("letrec")), bindings, call],
        Value::Empty,
    );

    // Return the new expression to be evaluated
    println!("{}", letrec);
    Ok(letrec)
}

// Binding Helpers ////////////////////////////////////////////////////////////

pub fn bind_closure_args(
    closure: Rc<Closure<Value>>,
    args: &Value,
) -> Result<Rc<Env<Str, Value>>, Error> {
    println!("bind-closure-args -- {}", args);
    let new_env = Env::add_scope(closure.env.clone());
    match closure.formals.clone() {
        Formals::Collect(s) => new_env.insert((*s).clone(), args.clone()),
        Formals::Fixed(params) => {
            println!("bind closure args - fixed {:?}", params);
            if !params.is_empty() {
                let mut iter = Value::get_pair_cell(args)
                    .ok_or(Error::ArgsNotList)?
                    .values();
                for s in params.into_iter() {
                    match iter.next() {
                        Some(val) => new_env.insert((*s).clone(), val),
                        None => return Err(Error::Arity),
                    }
                }
            }
            // TODO what if the params are empty, but the args are not? do
            // we error on fixed?
        }
        Formals::Rest(params, symbol) => {
            println!("{:?}, {:?}", params, symbol);
            let mut iter = Value::get_pair_cell(args)
                .ok_or(Error::ArgsNotList)?
                .cells();
            let mut next_cell = iter.next();
            for s in params.into_iter() {
                match next_cell.clone() {
                    Some(val) => new_env.insert((*s).clone(), val.head().clone()),
                    None => return Err(Error::Arity),
                }
                next_cell = iter.next();
                println!("{:?}", next_cell);
            }
            let last = match next_cell {
                Some(val) => Value::from(val),
                None => Value::Empty,
            };
            new_env.insert((*symbol).clone(), last);
        }
    }
    Ok(new_env)
}

fn unbind(list: Value) -> Result<(Vec<Rc<Str>>, Vec<Value>), Error> {
    let cell = Value::get_pair_cell(&list).ok_or(Error::ArgsNotList)?;
    let mut strings = Vec::new();
    let mut args = Vec::new();
    for val in cell.values() {
        let (var, arg) = utils::fixed_take_2(&val)?;
        strings.push(match var {
            Value::Symbol(s) => s,
            _ => return Err(Error::BadArg(1)),
        });
        args.push(arg);
    }
    Ok((strings, args))
}

fn unbind_do(list: Value) -> Result<(Value, Value, Value), Error> {
    let cell = Value::get_pair_cell(&list).ok_or(Error::ArgsNotList)?;
    let mut symbols = Vec::new();
    let mut initial = Vec::new();
    let mut steps = Vec::new();

    for val in cell.values() {
        let (var, init, step) = utils::opt_last_take_3(&val)?;
        match var {
            Value::Symbol(_) => symbols.push(var.clone()),
            _ => return Err(Error::BadArg(1)),
        };
        initial.push(init);
        match step {
            Some(s) => steps.push(s),
            None => steps.push(var),
        }
    }

    Ok((
        Value::list_from_vec(symbols, Value::Empty),
        Value::list_from_vec(initial, Value::Empty),
        Value::list_from_vec(steps, Value::Empty),
    ))
}
