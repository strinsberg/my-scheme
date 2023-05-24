use my_scheme_compiler::compile::compile_string;
use my_scheme_interpreter::env::Env;
use my_scheme_interpreter::err::Error;
use my_scheme_interpreter::number_procs as np;
use my_scheme_interpreter::proc::{CompClos, Formals};
use my_scheme_interpreter::string::Str;
use my_scheme_interpreter::value::Value;
use std::rc::Rc;

fn sum(args: Vec<Value>) -> Result<Value, Error> {
    np::add(Value::list_from_vec(args, Value::Empty))
}

pub fn bind_closure_args(
    closure: Rc<CompClos<Value>>,
    args: &Value,
) -> Result<Rc<Env<Str, Value>>, Error> {
    let new_env = Env::add_scope(closure.env.clone());
    match closure.formals.clone() {
        Formals::Collect(s) => new_env.insert((*s).clone(), args.clone()),
        Formals::Fixed(params) => {
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
        }
        Formals::Rest(params, symbol) => {
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

fn apply(func: Value, args: Vec<Value>) -> Result<Value, Error> {
    match func {
        Value::CompClos(ref clos) => {
            let env = bind_closure_args(clos.clone(), &Value::list_from_vec(args, Value::Empty))?;
            let func = clos.func;
            func(env)
        }
        _ => panic!("only does CompClos right now"),
    }
}

pub fn lambda(
    name: Option<Str>,
    env: Rc<Env<Str, Value>>,
    formals: Formals,
    func: fn(Rc<Env<Str, Value>>) -> Result<Value, Error>,
) -> Value {
    Value::CompClos(Rc::new(CompClos::new(name, env, formals, func)))
}

fn test_1() {
    // (+ 1 2 3)
    println!(
        "{}",
        sum(vec![Value::from(1), Value::from(2), Value::from(3)]).unwrap()
    )
}

fn test_2() {
    // ((lambda (x) (+ 1 x)) 11)
    println!(
        "{}",
        apply(
            lambda(
                None,
                Rc::new(Env::new()),
                Formals::Fixed(vec![Rc::new(Str::from("x"))]),
                |env| sum(vec![Value::from(1), env.lookup(&Str::from("x")).unwrap()]),
            ),
            vec![Value::from(11)]
        )
        .unwrap()
    );
}

fn test_3() {
    println!(
        "{}",
        {
            let a = Value::from(5);
            sum(vec![Value::from(4), a])
        }
        .unwrap()
    );
}

fn test_4() {
    println!("{}", compile_string("5").unwrap());
}

fn test_5() {
    println!("{}", compile_string("(cons 5 6)").unwrap());
}

fn test_6() {
    println!(
        "{}",
        compile_string("(let ((a 4) (b 5)) (cons 1 2) (cons a b))").unwrap()
    );
}

fn test_7() {
    println!("{}", compile_string("(if #t 1)").unwrap());
}

fn test_8() {
    println!("{}", compile_string("(if (cons 3 4) 1 2)").unwrap());
}

fn test_9() {
    println!(
        "{}",
        compile_string("(lambda (x y) (cons y x) (cons x y))").unwrap()
    );
}

fn test_10() {
    println!("{}", compile_string("(f 1 2)").unwrap());
}

fn test_11() {
    println!(
        "{}",
        compile_string("((lambda (x y) (cons x y)) 1 2)").unwrap()
    );
}

fn test_12() {
    println!(
        "{}",
        compile_string("(letrec ((f (lambda (x y) (cons x y))) (a 5)) (cons a 2))").unwrap()
    );
}

fn main() {
    test_1();
    test_2();
    test_3();
    println!("");
    test_4();
    println!("");
    test_5();
    println!("");
    test_6();
    println!("");
    test_7();
    println!("");
    test_8();
    println!("");
    test_9();
    println!("");
    test_10();
    println!("");
    test_11();
    println!("");
    test_12();
}

/*
 * With some knowledge from the compiler and some modifications to how some
 * structures work this is totally possible. I am honestly not even sure the
 * env is necessary, as rust has closures too. So it may be possible to just use
 * variables for names. The only difficulty with that is making sure that a variable
 * can be used when necessary and a symbol when necessary. Probably the distinction
 * is between quoted and unquoted symbols, but what happens when a quoted symbol
 * is passed around and evaluated, or is that even possible for a quoted symbol
 * without eval?
 */

/*
* (define y 4)
* (define add4 (lambda (x) (+ x y)))
* (add4 8)
*
* let y = 4;
* let add4 = lambda(
*     name,
*     |args| {
*         arity(name, args, 1)?;
*         sum(vec![args[0].clone(), y.clone()])
* });
* apply(add4, vec![8]);
*
*
* (let ((a 5)) (+ 4 a))
* ((lambda (a) (+ 4 a)) 5)
*
* apply(
*     lambda(
*         name,
*         |args| {
*             arity(name, args, 1)?;
*             sum(vec![Value::from(4), args[0].clone()])
*     }),
*     Value::from(5));
*
* OR....
*
* {
*     let a = Value::from(5);
*     sum(vec![Value::from(4), a])
* }
*
*
* This is here to show that this is difficult when objects will be passed around
* as first class values. Sure we can create the function instead of a lambda
* struct or a closure, but then we have to have a Value variant with a fn pointer
* and no name. The other way can save the name and still have a closure that
* should not be much more work. Plus this only works for top level functions.
* If it is faster since it evaluates the function at compile time, rather than
* building a closure at runtime? then it might be worth it to explore. We could
* call it just f as well, but would have to keep track of different kinds of
* functions to ensure we were doing it right.
*
* (define (f x) (+ x 1))
*
* pub fn ___f(args: Vec<Value>) -> Result<Value, Error> {
*    let x = args[0].clone();
*    sum(vec![x, Value::from(1)])
* }
* let f = Value::from(___f)
*
*/
