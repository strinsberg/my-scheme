use my_scheme_lib::compile::core::*;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::list;
fn program() -> Result<Value, Error> {
let env = new_env();
Ok(Value::from(vec![Value::from(1), list![Value::from(4), symbol("a"), symbol("+")], Value::from(3)]))
}
fn main() {
    match program() {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
    }
}