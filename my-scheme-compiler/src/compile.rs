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

// TODO some simple tests are ok in here, but because we do not introduce line
// breaks into the final compilations and use some spacing that is not standard
// it is really hard to ensure the result is as expected. SO put more tests in
// the tests/compiler_tests.rs to test more complex behaviour directly.
// Or at least use the compilation in tests to determine what the right output
// is that will compile before trying to perfect the test string, and know that
// any changes to the compilation of certain things could break all of the
// unit tests.
// TODO add more special forms. Really all closures and buitin procs are just
// called with apply, so special forms are all that we have to add here.
// TODO some of these use the same code over and over again when they could call
// one of the other compile functions, try and clean it up.
// TODO the major inefficiency built into the method that I am using is symbol
// lookup. This is not so much because of the lookup operations, but because everytime
// I pass a string to those functions it must create a new Str. The best way
// to deal with this imo is to create a vector of all the symbols encountered
// during a program run and construct them all at the start of the program.
// Then anywhere they are used they can be passed by reference or cloned. The
// env functions can take them by reference or clone (cloning is copying the Rc)
// which will avoid creating a new string, just using the string inside the
// Symbol as a key. Might even make sense to make the keys Rc<Str> so that they
// can be shared rather than copying strings for every put. If a symbol is used
// quoted it can simply use the variable.clone() to put the symbol itself into
// the program. Again, every quoted symbol would be creating a new Rc<Str> rather
// than copying a pointer like it would in the interpreter. This is a waste, so
// cloning an Rc is much better.
// To do this the compiler will need to be a struct again that keeps record of
// all symbols seen in a vector. Then when compiling the program the first thing
// will be to let var = symbol("var"); for all the symbols. Then use
// "var" with var or &var or var.clone() in the program.
// Since the symbol names might be unprintable with rust it will be better to
// map a number to the symbol and use that as the identifier in the program.
// Instead of directly moving the symbols to the vec, we can use a function that
// puts the name in a map and gives it a unique int. So if a symbol is compiled
// and is not in the map it will insert itself and use the returned var name,
// if it is in the map it will use the associated var name.

// Symbol List ////////////////////////////////////////////////////////////////

pub struct SymbolList {
    map: HashMap<String, u64>,
    idx: u64,
}

impl SymbolList {
    pub fn new() -> SymbolList {
        SymbolList {
            map: HashMap::new(),
            idx: 0,
        }
    }

    pub fn lookup(&self, sym: &String) -> Option<String> {
        if self.map.contains_key(sym) {
            Some(format!("__{}", self.map[sym]))
        } else {
            None
        }
    }

    pub fn add(&mut self, sym: &String) -> String {
        self.idx += 1;
        self.map.insert(sym.clone(), self.idx);
        format!("__{}", self.idx)
    }
}

// Compiler ///////////////////////////////////////////////////////////////////

pub struct ModuleCompiler {
    code: String,
    loads: Vec<String>,
    symbols: SymbolList,
}

impl ModuleCompiler {
    pub fn new(code: &str) -> ModuleCompiler {
        ModuleCompiler {
            code: code.to_string(),
            loads: Vec::new(),
            symbols: SymbolList::new(),
        }
    }

    pub fn compile_mod(&mut self) -> Result<String, CompileError> {
        let lines = vec![
            format!("{}", self.compile_use()?),
            "pub fn mod(env: Environment) {".to_string(),
            format!("{}", self.compile_symbols()?),
            format!("{}", self.compile_code()?),
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
        for ld in self.loads.iter() {
            lines.push(format!("mod {};", ld));
            lines.push(format!("use crate::{};", ld))
        }
        Ok(lines.join("\n"))
    }

    pub fn compile_symbols(&self) -> Result<String, CompileError> {
        let mut lines = Vec::new();
        for (name, index) in self.symbols.map.iter() {
            lines.push(format!(
                "let __{index} = Value::symbol_from_str(\"{name}\");"
            ));
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
            Ok(format!("Value::from({b})"))
        } else {
            Ok(format!("Ok(Value::from({b}))"))
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
        Ok(format!("get(env, &{var}){q}"))
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
            "apply( get(env, &{})?, vec![{}] ){q}",
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
                        "put(env, &{}, args[{i}].clone());",
                        self.get_var(&s.to_string())
                    ));
                    params.push(s)
                }
                _ => panic!("params should be symbols"),
            };
        }

        let q = if qmark { "?" } else { "" };
        Ok(format!(
            "Value::lambda(None, env.clone(), |args, env| {{ {} {} }}){q}",
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
            "put(env, &{var}, Value::Undefined); set(env, &{var}, {})?;",
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
            "set(env, &{var}, {}){q}",
            self.compile(args[1].clone(), true)?
        ))
    }

    // Derived Expressions //

    fn compile_let(&mut self, args: &[Value], qmark: bool) -> Result<String, CompileError> {
        if args.len() < 2 {
            return Err(CompileError::Agh);
        }

        let mut strings = vec!["{ let env = push(env); ".to_string()];
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
                    strings.push(format!("put(env, &{}, ", self.get_var(&s.to_string())))
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

        let mut strings = vec!["{ let env = push(env); ".to_string()];
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
                    "put(env, &{}, Value::Undefined); ",
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
                    strings.push(format!("set(env, &{}, ", self.get_var(&s.to_string())))
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
                "put(env, &{}, {})",
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
            "let env = push(env);".to_string(),
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
            Ok(format!("{var}.clone()"))
        } else {
            Ok(format!("Ok({var}.clone())"))
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
        if let Some(var) = self.symbols.lookup(name) {
            return var;
        }
        self.symbols.add(name)
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
            "get(env, &__1)".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'cons").compile_code().unwrap(),
            "Ok(__1.clone())".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("(+ 'a)").compile_code().unwrap(),
            "apply( get(env, &__2)?, vec![__1.clone()] )".to_string(),
        );
    }

    #[test]
    fn test_compile_bool() {
        assert_eq!(
            ModuleCompiler::new("#t").compile_code().unwrap(),
            "Ok(Value::from(true))".to_string(),
        );
        assert_eq!(
            ModuleCompiler::new("'#t").compile_code().unwrap(),
            "Ok(Value::from(true))".to_string(),
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
            "{ Value::from(1); get(env, &__1)?; get(env, &__2) }".to_string()
        );
    }

    #[test]
    fn test_compile_define_var() {
        assert_eq!(
            ModuleCompiler::new("(define a 1)").compile_code().unwrap(),
            "put(env, &__1, Value::Undefined); set(env, &__1, Value::from(1))?;".to_string(),
        );
        // TODO test with lambdas when ready
    }

    #[test]
    fn test_compile_set_var() {
        assert_eq!(
            ModuleCompiler::new("(set! a 1)").compile_code().unwrap(),
            "set(env, &__1, Value::from(1))".to_string(),
        );
    }

    #[test]
    fn test_compile_do() {
        assert_eq!(
            ModuleCompiler::new("(do ((i 0 (+ i 1))) ((= i 10) i) 1 2 3)")
                .compile_code()
                .unwrap(),
            vec![
                "{ let env = push(env);".to_string(),
                "put(env, &__1, Value::from(0));".to_string(),
                "loop { if".to_string(),
                "apply( get(env, &__3)?, vec![get(env, &__1)?, Value::from(10)] )?.is_true()"
                    .to_string(),
                "{ break; }".to_string(),
                "Value::from(1); Value::from(2); Value::from(3);".to_string(),
                "set(env, &__1,".to_string(),
                "apply( get(env, &__2)?, vec![get(env, &__1)?, Value::from(1)] )?)?;".to_string(),
                "} get(env, &__1) }".to_string(),
            ]
            .join(" ")
        );
    }

    #[test]
    fn test_compile_quoted_array() {
        assert_eq!(
            ModuleCompiler::new("'#(1 a 3)").compile_code().unwrap(),
            "Ok(Value::from(vec![Value::from(1), __1.clone(), Value::from(3)]))".to_string(),
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
            "Ok(list![Value::from(3), list![Value::from(4), __2.clone(), __1.clone()], Value::from(1)])".to_string(),
        );
    }
}
