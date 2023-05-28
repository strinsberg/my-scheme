use crate::error::CompileError;
use my_scheme_lib::data::array::Array;
use my_scheme_lib::data::cell::Cell;
use my_scheme_lib::data::char::Char;
use my_scheme_lib::data::number::Num;
use my_scheme_lib::data::rep::ExternalRep;
use my_scheme_lib::data::string::Str;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::io::reader::StringReader;
use std::collections::HashMap;
use std::rc::Rc;

// TODO a load statment will require creating a new ModuleCompiler and recursively
// compiling a module. The difficulty is that the modules it loads will have to
// be passed out to the higher modules, but only as use statments for the containing
// module. Also we do not want to compile a module twice, even though we would just
// replace it and write it once. So if the file name is in modules ignore it
// otherwise compile it and combine it's modules with yours. The key is the file
// name and the value is the compiled module code.
// TODO add more special forms. Really all closures and buitin procs are just
// called with apply, so special forms are all that we have to add here.
// TODO some of these use the same code over and over again when they could call
// one of the other compile functions, try and clean it up.
// TODO remove panics that are compile errors and add proper errors for the user.
// TODO need some kind of force-all for lists that have promises in them. Would
// force the head and the tail and set the contents of the cons cell or maybe
// just create a new one.

// Compiler ///////////////////////////////////////////////////////////////////

pub struct ModuleCompiler {
    code: String,
    using: Vec<String>,
    pub modules: HashMap<String, String>,
}

impl ModuleCompiler {
    pub fn new(code: &str) -> ModuleCompiler {
        ModuleCompiler {
            code: code.to_string(),
            using: Vec::new(),
            modules: HashMap::new(),
        }
    }

    pub fn compile_mod(&mut self) -> Result<String, CompileError> {
        let code = self.compile_code()?;
        let lines = vec![
            format!("{}", self.compile_use()?),
            "pub fn module(env: Environment) -> Result<Value, Error> {".to_string(),
            format!("{}", code),
            "}".to_string(),
        ];
        Ok(lines.join("\n"))
    }

    pub fn compile_use(&self) -> Result<String, CompileError> {
        let mut lines = vec![
            "use my_scheme_lib::data::value::Value;".to_string(),
            "use my_scheme_lib::data::err::Error;".to_string(),
            "use my_scheme_lib::proc::help::*;".to_string(),
            "use my_scheme_lib::list;".to_string(),
        ];
        for lib in self.using.iter() {
            lines.push(format!("mod {};", lib));
            lines.push(format!("use crate::{};", lib))
        }
        Ok(lines.join("\n"))
    }

    pub fn compile_code(&mut self) -> Result<String, CompileError> {
        let forms = StringReader::new(&self.code)
            .read_forms()
            .or(Err(CompileError::ReadError))?;
        let mut strings = Vec::new();
        for f in forms.iter() {
            strings.push(self.compile(f.clone(), false)?);
        }
        Ok(strings.join("\n"))
    }

    // Value Compilation //

    fn compile(&mut self, val: Value, qmark: bool) -> Result<String, CompileError> {
        match val {
            Value::Bool(b) => self.compile_bool(b, qmark),
            Value::Number(n) => self.compile_number(n, qmark),
            Value::Char(n) => self.compile_char(n, qmark),
            Value::String(n) => self.compile_str(n, qmark),
            Value::Symbol(s) => self.compile_symbol(s, qmark),
            Value::Pair(cell) => self.compile_list(cell, qmark),
            Value::Array(arr) => self.compile_array(arr, qmark),
            Value::Empty => self.compile_empty(qmark),
            _ => Err(CompileError::Agh),
        }
    }

    fn compile_bool(&mut self, b: bool, qmark: bool) -> Result<String, CompileError> {
        if qmark {
            Ok(format!("Value::Bool({b})"))
        } else {
            Ok(format!("Ok(Value::Bool({b}))"))
        }
    }

    fn compile_empty(&mut self, qmark: bool) -> Result<String, CompileError> {
        if qmark {
            Ok("Value::Empty)".to_string())
        } else {
            Ok("Ok(Value::Empty)".to_string())
        }
    }

    fn compile_number(&mut self, num: Num, qmark: bool) -> Result<String, CompileError> {
        if qmark {
            match num {
                Num::Int(i) => Ok(format!("Value::from({i})")),
                Num::Flt(f) => Ok(format!("Value::from({f})")),
                Num::Rat(a, b) => Ok(format!("Value::from(({a}, {b}))")),
            }
        } else {
            match num {
                Num::Int(i) => Ok(format!("Ok(Value::from({i}))")),
                Num::Flt(f) => Ok(format!("Ok(Value::from({f}))")),
                Num::Rat(a, b) => Ok(format!("Ok(Value::from(({a}, {b})))")),
            }
        }
    }

    fn compile_char(&mut self, ch: Char, qmark: bool) -> Result<String, CompileError> {
        if qmark {
            Ok(format!("Value::from('{}')", ch.to_byte() as char))
        } else {
            Ok(format!("Ok(Value::from('{}'))", ch.to_byte() as char))
        }
    }

    fn compile_str(&mut self, s: Rc<Str>, qmark: bool) -> Result<String, CompileError> {
        if qmark {
            Ok(format!("Value::from({})", s.to_external()))
        } else {
            Ok(format!("Ok(Value::from({}))", s.to_external()))
        }
    }

    fn compile_symbol(&mut self, name: Rc<Str>, qmark: bool) -> Result<String, CompileError> {
        let var = self.get_var(&name.to_string());
        let q = if qmark { "?" } else { "" };
        Ok(format!("get(&env, {var}){q}"))
    }

    fn compile_list(&mut self, cell: Rc<Cell<Value>>, qmark: bool) -> Result<String, CompileError> {
        let args: Vec<Value> = cell.values().collect();
        match args[0].clone() {
            Value::Pair(_) => self.compile_apply_pair(args[0].clone(), &args[1..], qmark),
            Value::Symbol(s) => match s.to_string().as_str() {
                "quote" => self.compile_quoted(args[1].clone(), qmark),
                "lambda" => self.compile_lambda(&args[1..], qmark),
                "if" => self.compile_if(&args[1..]),
                "define" => self.compile_define(&args[1..]),
                "set!" => self.compile_set(&args[1..], qmark),
                "let" => self.compile_let(&args[1..], qmark),
                "letrec" => self.compile_letrec(&args[1..], qmark),
                "begin" => self.compile_begin(&args[1..], qmark),
                "do" => self.compile_do(&args[1..], qmark),
                "promise" => self.compile_promise(&args[1..], qmark),
                "force" => self.compile_force(&args[1..], qmark),
                // TODO quote, do, and, or, cond, case, apply
                _ => self.compile_apply_symbol(s, &args[1..], qmark),
            },
            _ => return Err(CompileError::Agh),
        }
    }

    fn compile_array(
        &mut self,
        arr: Rc<Array<Value>>,
        qmark: bool,
    ) -> Result<String, CompileError> {
        let mut values = Vec::new();
        for v in arr.values() {
            values.push(self.compile(v, true)?);
        }

        if qmark {
            Ok(format!("Value::from(vec![{}])", values.join(", ")))
        } else {
            Ok(format!("Ok(Value::from(vec![{}]))", values.join(", ")))
        }
    }

    // Procedure Application //

    fn compile_apply_pair(
        &mut self,
        func: Value,
        args: &[Value],
        qmark: bool,
    ) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone(), true)?);
        }

        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "apply( {}, vec![{}] ){q}",
            self.compile(func, true)?,
            strings.join(", ")
        ))
    }

    fn compile_apply_symbol(
        &mut self,
        s: Rc<Str>,
        args: &[Value],
        qmark: bool,
    ) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for val in args.iter() {
            strings.push(self.compile(val.clone(), true)?);
        }

        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "apply( get(&env, {})?, vec![{}] ){q}",
            self.get_var(&s.to_string()),
            strings.join(", ")
        ))
    }

    // Special Forms

    // TODO Add name param for when this is called in define or let expr
    // TODO this does not check arity and therefore will panic if the args vec
    // passed is not the right size. It also does not account for collect or rest
    // args which need to slice the vec and turn it into a list to save as the
    // value in the env for rest or collect vars.
    fn compile_lambda(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let cell = Value::get_pair_cell(&args[0]).expect("params is pair");
        let mut assignments = Vec::new();
        let mut params = Vec::new();
        for (i, p) in cell.values().enumerate() {
            match p {
                Value::Symbol(s) => {
                    assignments.push(format!(
                        "put(&env, {}, args[{i}].clone());",
                        self.get_var(&s.to_string())
                    ));
                    params.push(s)
                }
                _ => panic!("params should be symbols"),
            };
        }

        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "lambda(None, env.clone(), |args, env| {{ {} {} }}){q}",
            assignments.join(" "),
            self.compile_body(&args[1..])?
        ))
    }

    fn compile_if(&mut self, args: &[Value]) -> Result<String, CompileError> {
        match args.len() {
            3 => Ok(format!(
                "if {}.is_true() {{ {} }} else {{ {} }}",
                self.compile(args[0].clone(), true)?,
                self.compile(args[1].clone(), false)?,
                self.compile(args[2].clone(), false)?
            )),
            2 => Ok(format!(
                "if {}.is_true() {{ {} }}",
                self.compile(args[0].clone(), true)?,
                self.compile(args[1].clone(), false)?
            )),
            _ => Err(CompileError::Agh),
        }
    }

    fn compile_define(&mut self, args: &[Value]) -> Result<String, CompileError> {
        let var = match args[0].clone() {
            Value::Symbol(s) => self.get_var(&s.to_string()),
            // TODO if it is a pair that means it is a lambda define
            _ => panic!("define should have symbol"), // TODO make Err
        };

        Ok(format!(
            "put(&env, {var}, Value::Undefined); set(&env, {var}, {})?;",
            self.compile(args[1].clone(), true)?
        ))
    }

    fn compile_set(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        let var = match args[0].clone() {
            Value::Symbol(s) => self.get_var(&s.to_string()),
            // TODO if it is a pair that means it is a lambda define
            _ => panic!("define should have symbol"), // TODO make Err
        };

        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "set(&env, {var}, {}){q}",
            self.compile(args[1].clone(), true)?
        ))
    }

    fn compile_promise(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "promise(env.clone(), |env| {{ {} }}){q}",
            self.compile(args[0].clone(), false)?
        ))
    }

    fn compile_force(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "force({}){q}",
            self.compile(args[0].clone(), true)?
        ))
    }

    // Derived Expressions //

    fn compile_let(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
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
            match bind[0] {
                Value::Symbol(ref s) => {
                    strings.push(format!("put(&env, {}, ", self.get_var(&s.to_string())))
                }
                _ => return Err(CompileError::Agh),
            }
            // TODO here is where we would pass a name if the argument was a lambda
            // but we have to check it is a lambda and not a symbol or another
            // expression and either compile directly or compile_lambda
            strings.push(self.compile(bind[1].clone(), true)?);
            strings.push("); ".to_string());
        }
        strings.push(self.compile_body(&args[1..])?);

        let q = if qmark { "?" } else { "" };
        strings.push(format!(" }}{q}"));
        Ok(strings.join(""))
    }

    fn compile_letrec(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
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
            match bind[0] {
                Value::Symbol(ref s) => strings.push(format!(
                    "put(&env, {}, Value::Undefined); ",
                    self.get_var(&s.to_string())
                )),
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
            match bind[0] {
                Value::Symbol(ref s) => {
                    strings.push(format!("set(&env, {}, ", self.get_var(&s.to_string())))
                }
                _ => return Err(CompileError::Agh),
            }
            // TODO here is where we would pass a name if the argument was a lambda
            // but we have to check it is a lambda and not a symbol or another
            // expression and either compile directly or compile_lambda
            strings.push(self.compile(bind[1].clone(), true)?);
            strings.push(")?; ".to_string());
        }
        strings.push(self.compile_body(&args[1..])?);

        let q = if qmark { "?" } else { "" };
        strings.push(format!(" }}{q}"));
        Ok(strings.join(""))
    }

    fn compile_begin(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        let q = if qmark { "?" } else { "" };
        Ok(format!("{{ {} }}{q}", self.compile_body(args)?))
    }

    fn compile_do(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        let bindings: Vec<Vec<Value>> = match args[0].clone() {
            Value::Pair(cell) => cell
                .values()
                .map(|val| match val {
                    Value::Pair(cell) => cell.values().collect::<Vec<Value>>(),
                    _ => panic!("all bindings should be a list"),
                })
                .collect(),
            _ => panic!("should give list of lists for first arg to do"),
        };

        let mut inits = Vec::new();
        let mut steps = Vec::new();
        for b in bindings.iter() {
            inits.push(format!(
                "put(&env, {}, {})",
                self.get_var(&b[0].to_string()),
                self.compile(b[1].clone(), true)?
            ));
            if b.len() > 2 {
                steps.push(self.compile_set(&vec![b[0].clone(), b[2].clone()], true)?);
            }
        }

        let cond = match args[1].clone() {
            Value::Pair(cell) => cell.values().collect::<Vec<Value>>(),
            _ => panic!("should give list of lists for first arg to do"),
        };

        let mut body = Vec::new();
        for val in args[2..].iter() {
            body.push(self.compile(val.clone(), true)?);
        }

        let q = if qmark { "?" } else { "" };
        let lines = vec![
            "{".to_string(),
            "let env = push(&env);".to_string(),
            format!("{};", inits.join("; ")),
            format!(
                "loop {{ if {}.is_true() {{ break; }}",
                self.compile(cond[0].clone(), true)?
            ),
            format!("{};", body.join("; ")),
            format!("{};", steps.join("; ")),
            "}".to_string(),
            self.compile(cond[1].clone(), false)?,
            format!("}}{q}"),
        ];
        Ok(lines.join(" "))
    }

    // Quoted compilation //

    fn compile_quoted(&mut self, val: Value, qmark: bool) -> Result<String, CompileError> {
        match val {
            Value::Bool(b) => self.compile_bool(b, qmark),
            Value::Number(n) => self.compile_number(n, qmark),
            Value::Char(n) => self.compile_char(n, qmark),
            Value::String(n) => self.compile_str(n, qmark),
            Value::Symbol(s) => self.compile_quoted_symbol(s, qmark),
            Value::Pair(cell) => self.compile_quoted_list(cell, qmark),
            Value::Array(arr) => self.compile_quoted_array(arr, qmark),
            Value::Empty => self.compile_empty(qmark),
            _ => Err(CompileError::Agh),
        }
    }

    fn compile_quoted_symbol(&mut self, s: Rc<Str>, qmark: bool) -> Result<String, CompileError> {
        let var = self.get_var(&s.to_string());
        if qmark {
            Ok(format!("Value::symbol_from_str({var})"))
        } else {
            Ok(format!("Ok(Value::symbol_from_str({var}))"))
        }
    }

    fn compile_quoted_list(
        &mut self,
        c: Rc<Cell<Value>>,
        qmark: bool,
    ) -> Result<String, CompileError> {
        let mut values = Vec::new();
        for v in c.values() {
            values.push(self.compile_quoted(v, true)?);
        }
        values = values.into_iter().rev().collect();

        if qmark {
            Ok(format!("list![{}]", values.join(", ")))
        } else {
            Ok(format!("Ok(list![{}])", values.join(", ")))
        }
    }

    fn compile_quoted_array(
        &mut self,
        arr: Rc<Array<Value>>,
        qmark: bool,
    ) -> Result<String, CompileError> {
        let mut values = Vec::new();
        for v in arr.values() {
            values.push(self.compile_quoted(v, true)?);
        }

        if qmark {
            Ok(format!("Value::from(vec![{}])", values.join(", ")))
        } else {
            Ok(format!("Ok(Value::from(vec![{}]))", values.join(", ")))
        }
    }

    // Helpers //

    fn get_var(&mut self, name: &String) -> String {
        // TODO kind of unecessary, but works for the moment to avoid rewriting
        // all the symbols
        format!("\"{name}\"")
    }

    fn compile_body(&mut self, args: &[Value]) -> Result<String, CompileError> {
        let mut strings = Vec::new();
        for (i, val) in args.iter().enumerate() {
            strings.push(self.compile(val.clone(), i != args.len() - 1)?);
        }
        Ok(strings.join("; "))
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_symbols() {
        assert_eq!(
            ModuleCompiler::new("cons").compile_code().unwrap(),
            "get(&env, \"cons\")".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'cons").compile_code().unwrap(),
            "Ok(Value::symbol_from_str(\"cons\"))".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("(+ 'a)").compile_code().unwrap(),
            "apply( get(&env, \"+\")?, vec![Value::symbol_from_str(\"a\")] )".to_string(),
        );
    }

    #[test]
    fn test_compile_bool() {
        assert_eq!(
            ModuleCompiler::new("#t").compile_code().unwrap(),
            "Ok(Value::Bool(true))".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'#t").compile_code().unwrap(),
            "Ok(Value::Bool(true))".to_string(),
        );
    }

    #[test]
    fn test_compile_char() {
        assert_eq!(
            ModuleCompiler::new("#\\space").compile_code().unwrap(),
            "Ok(Value::from(' '))".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'#\\P").compile_code().unwrap(),
            "Ok(Value::from('P'))".to_string(),
        );
    }

    #[test]
    fn test_compile_string() {
        assert_eq!(
            ModuleCompiler::new("\"1 \\n 2 3\"").compile_code().unwrap(),
            "Ok(Value::from(\"1 \\n 2 3\"))".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'\"1 \\n 2 3\"")
                .compile_code()
                .unwrap(),
            "Ok(Value::from(\"1 \\n 2 3\"))".to_string(),
        );
    }

    #[test]
    fn test_compile_begin() {
        assert_eq!(
            ModuleCompiler::new("(begin 1 a b)").compile_code().unwrap(),
            "{ Value::from(1); get(&env, \"a\")?; get(&env, \"b\") }".to_string()
        );
    }

    #[test]
    fn test_compile_define_var() {
        assert_eq!(
            ModuleCompiler::new("(define a 1)").compile_code().unwrap(),
            "put(&env, \"a\", Value::Undefined); set(&env, \"a\", Value::from(1))?;".to_string(),
        );
        // TODO test with lambdas when ready
    }

    #[test]
    fn test_compile_set_var() {
        assert_eq!(
            ModuleCompiler::new("(set! a 1)").compile_code().unwrap(),
            "set(&env, \"a\", Value::from(1))".to_string(),
        );
    }

    #[test]
    fn test_compile_do() {
        assert_eq!(
            ModuleCompiler::new("(do ((i 0 (+ i 1))) ((= i 10) i) 1 2 3)")
                .compile_code()
                .unwrap(),
            vec![
                "{ let env = push(&env);".to_string(),
                "put(&env, \"i\", Value::from(0));".to_string(),
                "loop { if".to_string(),
                "apply( get(&env, \"=\")?, vec![get(&env, \"i\")?, Value::from(10)] )?.is_true()"
                    .to_string(),
                "{ break; }".to_string(),
                "Value::from(1); Value::from(2); Value::from(3);".to_string(),
                "set(&env, \"i\",".to_string(),
                "apply( get(&env, \"+\")?, vec![get(&env, \"i\")?, Value::from(1)] )?)?;"
                    .to_string(),
                "} get(&env, \"i\") }".to_string(),
            ]
            .join(" ")
        );
    }

    #[test]
    fn test_compile_quoted_array() {
        assert_eq!(
            ModuleCompiler::new("'#(1 a 3)").compile_code().unwrap(),
            "Ok(Value::from(vec![Value::from(1), Value::symbol_from_str(\"a\"), Value::from(3)]))"
                .to_string(),
        );
    }

    #[test]
    fn test_compile_quoted_list() {
        // NOTE the list macro just builds the list starting with the first element
        // but this makes the result reversed, so we compile with the arguments
        // to list! in reverse so the resulting list built at runtime is in the
        // right order and does not have to do the reversal at runtime.
        assert_eq!(
            ModuleCompiler::new("'(1 (+ a 4) 3)").compile_code().unwrap(),
            "Ok(list![Value::from(3), list![Value::from(4), Value::symbol_from_str(\"a\"), Value::symbol_from_str(\"+\")], Value::from(1)])".to_string(),
        );
    }
}
