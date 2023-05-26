use my_scheme_lib::compile::core::*;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
fn program() -> Result<Value, Error> {
let env = new_env();
{ let env = push(&env); put(&env, "i", Value::from(0)); loop { if apply( get(&env, "=")?, vec![get(&env, "i")?, Value::from(10)] )?.is_true() { break; } Value::from(1); Value::from(2); Value::from(3); set(&env, "i", apply( get(&env, "+")?, vec![get(&env, "i")?, Value::from(1)] )?)?; } get(&env, "i") }
}
fn main() {
    match program() {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
    }
}