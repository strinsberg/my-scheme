use my_scheme_lib::data::value::Value;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::proc::help::*;
use my_scheme_lib::list;
pub fn module(env: Environment) -> Result<Value, Error> {
{ let env = push(&env); put(&env, "f", Value::Undefined); put(&env, "g", Value::Undefined); set(&env, "f", lambda(None, env.clone(), |args, env| { put(&env, "x", args[0].clone()); apply( get(&env, "g")?, vec![get(&env, "x")?] ) })?)?; set(&env, "g", lambda(None, env.clone(), |args, env| { put(&env, "x", args[0].clone()); apply( get(&env, "+")?, vec![Value::from(1), get(&env, "x")?] ) })?)?; apply( get(&env, "f")?, vec![Value::from(6)] ) }
}

fn main() {
    let env = new_env();
    match module(env) {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
   }
}