use crate::compile;
use crate::error::CompileError;

pub struct ProgramCompiler {
    in_path: String,
    out_path: String,
    loads: Vec<String>,
    symbols: Vec<String>,
}

impl ProgramCompiler {
    pub fn new(in_path: &str, out_path: &str) -> ProgramCompiler {
        ProgramCompiler {
            in_path: in_path.to_string(),
            out_path: out_path.to_string(),
            loads: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn compile(&self) -> Result<(), CompileError> {
        // load in the file
        // compile the contents
        // write the new file
        Ok(())
    }
}

/*

use my_scheme_lib::core::null_env;
use crate::somemodule;

fn main() {
    let env = null_env();
    match somemodule::mod(&env) {
        Ok(_) => (),
        Err(e) => println!("{}", e),
    }
}

*/

/*

use my_scheme_lib::core::*;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::list;
use crate::someothermod::mod;

pub fn mod(env: &Env) -> Result<Value, Error> {
    // make symbols
    let __1 = Value::symbol_from_str("+");

    // load mods
    someothermod::mod(env)?

    // program
    apply( get(env, __1), list![Value::from(2), Value::from(1)] )
}

*/
