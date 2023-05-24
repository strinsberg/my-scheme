use crate::data::cell::Cell;
use crate::data::number::Num;
use crate::data::string::Str;
use crate::data::value::Value;
use crate::io::reader::StringReader;
use std::rc::Rc;

// TODO this needs some things
// 1. The lib that things are compiled from needs a core.rs with the functions
//    used below added to it.
// 2. There are a couple Value additions that will need to be added to value,
//    specifically Value::lambda, and Value::from_rat.
// 3. For a set! function to work we will need to make all let stmts mut.
// 4. We need to maintain a list of proc symbols and information on them so
//    that builtin procs can be given the right rust identifier. I have been
//    thinking that something like a proc.rs module with all the actual
//    functions that take a vec of args. The list could just be a hash table
//    with key being the scheme name and the value being the rust name.
//    We do not need formals etc. as the proc module would wrap each function
//    in a call that would check arity and types at runtime.
//
// The only thing that I feel like is tricky is the actual compilation as a rust
// file. To compile this file one of two things would need to happen:
// 1. The librairy with all the data types must be in the users path so that they
//    can just run rustc or cargo build on the file/folder.
// 2. The library functions and data structures need to be copied into the
//    resulting file so that it can be compiled without dependencies.
//
// Number 1 leaves the compiled file the smallest and is easy to just wrap the
// compiled code in a main and import all of the necessary modules at the top.
// Number 2 is the most convenient for the user, but it will make very big files
// that have to include the whole std lib. The bonus of this side is that I could
// quite easily just put a shell call into the compiler to rustc the file for
// the user without them having to compile the resulting file themselves.
//
// Number 1 is probably a good solution provided the resulting file/folder is
// compiled into an environment that makes sense. It might need a folder so that
// cargo can specify where to get the dependencies. I probably could put them
// in a public folder on github and use git dependencies. The user would get a
// folder with a simple Cargo.toml and a main.rs. They could cd into it and call
// cargo build and get an executable.
//
// Number 2 would be simpler for the user and not require any github repos etc.
// but would be less usefull in some ways. Though it would have the advantage
// of things like my previous compilers compiling to a format that could be
// submitted anywhere rust could be submitted. Though probably with size
// restrictions it would not fit.
//
// TODO the other problem here is that the compiler probably needs some
// different files and structures that the interpreter does not. One way to deal
// with this is to split up the interpreter library into parts. One for the data
// and core functions. One for the extra functions. The compiler could then import
// what it needs, but it would have to add some things. Which would mean that
// we might have to create another module for compiler additions that would use
// the interpreter libs and create more things for the compiled files to use.
// This will get out of hand fast as everything will need to be separated and
// then built on continually. It might just be the best to put the compiler and
// the interpreter libraries into the lib and then create another binary that
// pulls in that lib for the compiler, like I was going to do for a repl and hav
// done for the webpage.

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    Agh,
}

pub fn compile_string(s: &str) -> Result<String, CompileError> {
    let forms = StringReader::new(s)
        .read_forms()
        .or(Err(CompileError::Agh))?;
    let mut strings = Vec::new();
    for f in forms.iter() {
        strings.push(compile(f.clone())?);
    }
    Ok(strings.join("\n"))
}

pub fn compile(val: Value) -> Result<String, CompileError> {
    match val {
        Value::Bool(b) => Ok(format!("Value::from({b})")),
        Value::Number(n) => compile_number(n),
        Value::Symbol(s) => Ok(s.to_string()),
        Value::Pair(cell) => compile_list(cell),
        Value::Empty => Ok("Value::Empty".to_string()),
        Value::Undefined => Ok("Value::Undefined".to_string()),
        _ => Err(CompileError::Agh),
    }
}

fn compile_list(cell: Rc<Cell<Value>>) -> Result<String, CompileError> {
    let args: Vec<Value> = cell.values().collect();
    match args[0].clone() {
        Value::Symbol(s) => match s.to_string().as_str() {
            //
            "car" => Ok(format!("core::car({})", compile(args[1].clone())?)),
            "cdr" => Ok(format!("core::cdr({})", compile(args[1].clone())?)),
            "cons" => Ok(format!(
                "core::cons({}, {})",
                compile(args[1].clone())?,
                compile(args[2].clone())?
            )),
            "lambda" => compile_lambda(&args[1..]),
            "let" => compile_let(&args[1..]),
            "letrec" => compile_letrec(&args[1..]),
            "if" => compile_if(&args[1..]),
            _ => compile_apply_symbol(s, &args[1..]),
        },
        Value::Pair(_) => compile_apply_pair(args[0].clone(), &args[1..]),
        _ => return Err(CompileError::Agh),
    }
}

fn compile_number(num: Num) -> Result<String, CompileError> {
    match num {
        Num::Int(i) => Ok(format!("Value::from({i})")),
        Num::Flt(f) => Ok(format!("Value::from({f})")),
        Num::Rat(a, b) => Ok(format!("Value::from_rat({a}, {b})")),
    }
}

fn compile_lambda(args: &[Value]) -> Result<String, CompileError> {
    if args.len() < 2 {
        return Err(CompileError::Agh);
    }

    let cell = Value::get_pair_cell(&args[0]).expect("params is pair");
    let mut strings = Vec::new();
    for (i, p) in cell.values().enumerate() {
        let asgn = match p {
            Value::Symbol(s) => format!("let {s} = args[{i}].clone();"),
            _ => panic!("params should be symbols"),
        };
        strings.push(asgn);
    }

    Ok(format!(
        "Value::lambda(|args| {{ {} {} }})",
        strings.join(" "),
        compile_body(&args[1..])?
    ))
}

fn compile_let(args: &[Value]) -> Result<String, CompileError> {
    if args.len() < 2 {
        return Err(CompileError::Agh);
    }

    let mut strings = vec!["{ ".to_string()];
    let cell = Value::get_pair_cell(&args[0]).expect("bindings should be list");
    for v in cell.values() {
        let bind: Vec<Value> = Value::get_pair_cell(&v)
            .expect("binding should be list")
            .values()
            .collect();
        if bind.len() != 2 {
            return Err(CompileError::Agh);
        }
        match bind[0].clone() {
            Value::Symbol(s) => strings.push(format!("let {s} = ")),
            _ => return Err(CompileError::Agh),
        }
        strings.push(compile(bind[1].clone())?);
        strings.push("; ".to_string());
    }
    strings.push(compile_body(&args[1..])?);
    strings.push(" }".to_string());
    Ok(strings.join(""))
}

fn compile_letrec(args: &[Value]) -> Result<String, CompileError> {
    if args.len() < 2 {
        return Err(CompileError::Agh);
    }

    let mut strings = vec!["{ ".to_string()];
    let cell = Value::get_pair_cell(&args[0]).expect("bindings should be list");
    // create undef bindings
    for v in cell.values() {
        let bind: Vec<Value> = Value::get_pair_cell(&v)
            .expect("binding should be list")
            .values()
            .collect();
        if bind.len() != 2 {
            return Err(CompileError::Agh);
        }
        match bind[0].clone() {
            Value::Symbol(s) => strings.push(format!("let mut {s} = Value::Undefined; ")),
            _ => return Err(CompileError::Agh),
        }
    }
    // create set bindings
    for v in cell.values() {
        let bind: Vec<Value> = Value::get_pair_cell(&v)
            .expect("binding should be list")
            .values()
            .collect();
        if bind.len() != 2 {
            return Err(CompileError::Agh);
        }
        match bind[0].clone() {
            Value::Symbol(s) => strings.push(format!("{s} = ")),
            _ => return Err(CompileError::Agh),
        }
        strings.push(compile(bind[1].clone())?);
        strings.push("; ".to_string());
    }
    strings.push(compile_body(&args[1..])?);
    strings.push(" }".to_string());
    Ok(strings.join(""))
}

fn compile_if(args: &[Value]) -> Result<String, CompileError> {
    match args.len() {
        3 => Ok(format!(
            "if {}.is_true() {{ {} }} else {{ {} }}",
            compile(args[0].clone())?,
            compile(args[1].clone())?,
            compile(args[2].clone())?
        )),
        2 => Ok(format!(
            "if {}.is_true() {{ {} }}",
            compile(args[0].clone())?,
            compile(args[1].clone())?
        )),
        _ => Err(CompileError::Agh),
    }
}

fn compile_body(args: &[Value]) -> Result<String, CompileError> {
    let mut strings = Vec::new();
    for val in args.iter() {
        strings.push(compile(val.clone())?);
    }
    Ok(strings.join("; "))
}

fn compile_apply_symbol(s: Rc<Str>, args: &[Value]) -> Result<String, CompileError> {
    // we have to lookup the symbol first?
    // if it is a builtin we will write proc::s(vec![args ...])
    let mut strings = Vec::new();
    for val in args.iter() {
        strings.push(compile(val.clone())?);
    }
    Ok(format!(
        "core::apply( {s}, vec![ {} ] )",
        strings.join(", ")
    ))
}

fn compile_apply_pair(func: Value, args: &[Value]) -> Result<String, CompileError> {
    let mut strings = Vec::new();
    for val in args.iter() {
        strings.push(compile(val.clone())?);
    }
    Ok(format!(
        "core::apply( {}, vec![ {} ] )",
        compile(func)?,
        strings.join(", ")
    ))
}
