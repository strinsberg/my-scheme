use crate::data::cell::Cell;
use crate::data::number::Num;
use crate::data::string::Str;
use crate::data::value::Value;
use crate::io::reader::StringReader;
use std::rc::Rc;

// TODO some simple tests are ok in here, but because we do not introduce line
// breaks into the final compilations and use some spacing that is not standard
// it is really hard to ensure the result is as expected. SO put more tests in
// the tests/compiler_tests.rs to test more complex behaviour directly.

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    ReadError,
    Agh,
}

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile_program_with_output(
        &mut self,
        c: &str,
        s: &str,
    ) -> Result<String, CompileError> {
        let lines = vec![
            // Imports
            format!("use {c}::compile::core::*;"),
            format!("use {c}::data::err::Error;"),
            format!("use {c}::data::value::Value;"),
            // Program function that includes compiled code
            "fn program() -> Result<Value, Error> {".to_string(),
            "let env = new_env();".to_string(),
            format!("{}", self.compile_string(s)?),
            "}".to_string(),
            // Main to call program and print result
            "fn main() {".to_string(),
            "    match program() {".to_string(),
            "        Ok(val) => println!(\"{val}\"),".to_string(),
            "        Err(e) => println!(\"{:?}\", e),".to_string(),
            "    }".to_string(),
            "}".to_string(),
        ];
        Ok(lines.join("\n"))
    }

    pub fn compile_program(&mut self, c: &str, s: &str) -> Result<String, CompileError> {
        let lines = vec![
            // Imports
            format!("use {c}::compile::core::*;"),
            format!("use {c}::data::err::Error;"),
            format!("use {c}::data::value::Value;"),
            // Program function that includes compiled code
            "fn program() -> Result<Value, Error> {".to_string(),
            format!("{}", self.compile_string(s)?),
            "}".to_string(),
            // Main to call program and print result
            "fn main() {".to_string(),
            "    match program() {".to_string(),
            "        Ok(_) => ()".to_string(),
            // TODO this will not work until error has a display fn
            "        Err(e) => println!(\"{e}\"),".to_string(),
            "    }".to_string(),
            "}".to_string(),
        ];
        Ok(lines.join("\n"))
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
        Ok(format!("get(&env, \"{}\")?", name.to_string()))
    }

    fn compile_list(&mut self, cell: Rc<Cell<Value>>) -> Result<String, CompileError> {
        let args: Vec<Value> = cell.values().collect();
        match args[0].clone() {
            Value::Symbol(s) => match s.to_string().as_str() {
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
            Num::Rat(a, b) => Ok(format!("rational({a}, {b})")),
        }
    }

    // TODO Add name param for when this is called in define or let expr
    // TODO this does not check arity and therefore will panic if the args vec
    // passed is not the right size. It also does not account for collect or rest
    // args which need to slice the vec and turn it into a list to save as the
    // value in the env for rest or collect vars.
    fn compile_lambda(&mut self, args: &[Value]) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let cell = Value::get_pair_cell(&args[0]).expect("params is pair");
        let mut assignments = Vec::new();
        let mut params = Vec::new();
        for (i, p) in cell.values().enumerate() {
            match p {
                Value::Symbol(s) => {
                    assignments.push(format!("put(&env, \"{s}\", args[{i}].clone());"));
                    params.push(s)
                }
                _ => panic!("params should be symbols"),
            };
        }

        Ok(format!(
            "lambda(None, env.clone(), |args, env| {{ {} {} }})",
            assignments.join(" "),
            self.compile_body(&args[1..])?
        ))
    }

    fn compile_let(&mut self, args: &[Value]) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let mut strings = vec!["{ let env = push(&env); ".to_string()];
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
                Value::Symbol(s) => strings.push(format!("put(&env, \"{s}\", ")),
                _ => return Err(CompileError::Agh),
            }
            // TODO here is where we would pass a name if the argument was a lambda
            // but we have to check it is a lambda and not a symbol or another
            // expression and either compile directly or compile_lambda
            strings.push(self.compile(bind[1].clone())?);
            strings.push("); ".to_string());
        }
        strings.push(self.compile_body(&args[1..])?);
        strings.push(" }".to_string());
        Ok(strings.join(""))
    }

    fn compile_letrec(&mut self, args: &[Value]) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let mut strings = vec!["{ let env = push(&env); ".to_string()];
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
                Value::Symbol(s) => strings.push(format!("put(&env, \"{s}\", Value::Undefined); ")),
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
                Value::Symbol(s) => strings.push(format!("set(&env, \"{s}\", ")),
                _ => return Err(CompileError::Agh),
            }
            // TODO here is where we would pass a name if the argument was a lambda
            // but we have to check it is a lambda and not a symbol or another
            // expression and either compile directly or compile_lambda
            strings.push(self.compile(bind[1].clone())?);
            strings.push(")?; ".to_string());
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
        Ok(format!(
            "apply( get(&env, \"{s}\")?, vec![{}] )",
            strings.join(", ")
        ))
    }

    fn compile_apply_pair(&mut self, func: Value, args: &[Value]) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone())?);
        }
        Ok(format!(
            "apply( {}, vec![{}] )",
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
    fn test_compile_symbol() {
        let mut comp = Compiler::new();
        assert_eq!(
            comp.compile_string("cons").unwrap(),
            "get(&env, \"cons\")?".to_string(),
        );
    }

    #[test]
    fn test_compile_apply_symbol() {
        let mut comp = Compiler::new();
        assert_eq!(
            comp.compile_string("(+ 1 2)").unwrap(),
            "apply( get(&env, \"+\")?, vec![Value::from(1), Value::from(2)] )".to_string()
        );
    }
}
