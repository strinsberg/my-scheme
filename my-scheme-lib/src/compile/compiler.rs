use crate::compile::proc_map;
use crate::data::cell::Cell;
use crate::data::number::Num;
use crate::data::string::Str;
use crate::data::value::Value;
use crate::io::reader::StringReader;
use std::collections::HashMap;
use std::rc::Rc;

// TODO test and clean up what we have and then add more
// TODO try and find a better way of testing the actual compiled code works
// maybe through a script in a tests folder.

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    ReadError,
    Agh,
}

pub struct Compiler {
    procs: HashMap<&'static str, &'static str>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            procs: proc_map::make_proc_map(),
        }
    }

    pub fn compile_program(&mut self, c: &str, s: &str) -> Result<String, CompileError> {
        let imports = vec![
            format!("use {c}::data::value::Value;"),
            format!("use {c}::compile::proc;"),
            format!("use {c}::compile::core;"),
        ];
        // TODO Once the compiler is used for something other than testing remove
        // the println!()
        // TODO the unwrap at the end will only work for compiling a single expr,
        // plus we need a way to deal with runtime errors at the top level. It
        // could be that every expr has to be evaluated in a match expr and
        // if it errors the program has to print the error and exit. Perhaps this
        // could be done to wrap each form compiled in compile_string()
        Ok(format!(
            "{}\n\nfn main () {{\nprintln!(\"{{}}\", {}.unwrap())\n}}",
            imports.join("\n"),
            self.compile_string(s)?
        ))
    }

    pub fn compile_string(&mut self, s: &str) -> Result<String, CompileError> {
        let forms = StringReader::new(s)
            .read_forms()
            .or(Err(CompileError::ReadError))?;
        let mut strings = Vec::new();
        for f in forms.iter() {
            strings.push(self.compile(f.clone())?);
        }
        Ok(strings.join("\n"))
    }

    pub fn compile(&mut self, val: Value) -> Result<String, CompileError> {
        match val {
            Value::Bool(b) => Ok(format!("Value::from({b})")),
            Value::Number(n) => self.compile_number(n),
            Value::Symbol(s) => self.compile_symbol(s),
            Value::Pair(cell) => self.compile_list(cell),
            Value::Empty => Ok("Value::Empty".to_string()),
            Value::Undefined => Ok("Value::Undefined".to_string()),
            _ => Err(CompileError::Agh),
        }
    }

    fn compile_symbol(&mut self, name: Rc<Str>) -> Result<String, CompileError> {
        match self.procs.get(name.to_string().as_str()) {
            Some(pname) => Ok(format!(
                "core::lambda( Some({name}), |args| {pname}(args) )"
            )),
            None => Ok(name.to_string()),
        }
    }

    fn compile_list(&mut self, cell: Rc<Cell<Value>>) -> Result<String, CompileError> {
        let args: Vec<Value> = cell.values().collect();
        match args[0].clone() {
            Value::Symbol(s) => match s.to_string().as_str() {
                //
                "car" => Ok(format!("core::car({})", self.compile(args[1].clone())?)),
                "cdr" => Ok(format!("core::cdr({})", self.compile(args[1].clone())?)),
                "cons" => Ok(format!(
                    "core::cons({}, {})",
                    self.compile(args[1].clone())?,
                    self.compile(args[2].clone())?
                )),
                "lambda" => self.compile_lambda(&args[1..]),
                "let" => self.compile_let(&args[1..]),
                "letrec" => self.compile_letrec(&args[1..]),
                "if" => self.compile_if(&args[1..]),
                _ => self.compile_apply_symbol(s, &args[1..]),
            },
            Value::Pair(_) => self.compile_apply_pair(args[0].clone(), &args[1..]),
            _ => return Err(CompileError::Agh),
        }
    }

    fn compile_number(&mut self, num: Num) -> Result<String, CompileError> {
        match num {
            Num::Int(i) => Ok(format!("Value::from({i})")),
            Num::Flt(f) => Ok(format!("Value::from({f})")),
            Num::Rat(a, b) => Ok(format!("core::rational({a}, {b})")),
        }
    }

    fn compile_lambda(&mut self, args: &[Value]) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let cell = Value::get_pair_cell(&args[0]).expect("params is pair");
        let mut strings = Vec::new();
        for (i, p) in cell.values().enumerate() {
            let asgn = match p {
                Value::Symbol(s) => format!("let mut {s} = args[{i}].clone();"),
                _ => panic!("params should be symbols"),
            };
            strings.push(asgn);
        }

        Ok(format!(
            "core::lambda(None, |args| {{ {} {} }})",
            strings.join(" "),
            self.compile_body(&args[1..])?
        ))
    }

    fn compile_let(&mut self, args: &[Value]) -> Result<String, CompileError> {
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
                Value::Symbol(s) => strings.push(format!("let mut {s} = ")),
                _ => return Err(CompileError::Agh),
            }
            strings.push(self.compile(bind[1].clone())?);
            strings.push("; ".to_string());
        }
        strings.push(self.compile_body(&args[1..])?);
        strings.push(" }".to_string());
        Ok(strings.join(""))
    }

    fn compile_letrec(&mut self, args: &[Value]) -> Result<String, CompileError> {
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
            strings.push(self.compile(bind[1].clone())?);
            strings.push("; ".to_string());
        }
        strings.push(self.compile_body(&args[1..])?);
        strings.push(" }".to_string());
        Ok(strings.join(""))
    }

    fn compile_if(&mut self, args: &[Value]) -> Result<String, CompileError> {
        match args.len() {
            3 => Ok(format!(
                "if {}.is_true() {{ {} }} else {{ {} }}",
                self.compile(args[0].clone())?,
                self.compile(args[1].clone())?,
                self.compile(args[2].clone())?
            )),
            2 => Ok(format!(
                "if {}.is_true() {{ {} }}",
                self.compile(args[0].clone())?,
                self.compile(args[1].clone())?
            )),
            _ => Err(CompileError::Agh),
        }
    }

    fn compile_body(&mut self, args: &[Value]) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone())?);
        }
        Ok(strings.join("; "))
    }

    fn compile_apply_symbol(&mut self, s: Rc<Str>, args: &[Value]) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone())?);
        }

        match self.procs.get(s.to_string().as_str()) {
            Some(name) => Ok(format!("{name}( vec![{}] )", strings.join(", "))),
            None => Ok(format!("core::apply( {s}, vec![{}] )", strings.join(", "))),
        }
    }

    fn compile_apply_pair(&mut self, func: Value, args: &[Value]) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone())?);
        }
        Ok(format!(
            "core::apply( {}, vec![{}] )",
            self.compile(func)?,
            strings.join(", ")
        ))
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_builtin() {
        let mut comp = Compiler::new();
        assert_eq!(
            comp.compile_string("(+ 1 2)").unwrap(),
            "proc::add( vec![Value::from(1), Value::from(2)] )".to_string()
        );
        // NOTE + is scanned as a peculiar identifier and they only return
        // properly when a delimeter byte is reached, but that does not include
        // eof, so "+" is a scanner error right now.
        assert_eq!(
            comp.compile_string(" + ").unwrap(),
            "core::lambda( Some(+), |args| proc::add(args) )".to_string()
        );
    }
}
