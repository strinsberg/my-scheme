use my_scheme_lib::compile::core::*;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::list;
fn program() -> Result<Value, Error> {
    let __1 = symbol("a");
    let __2 = symbol("+");
    let env = new_env();
    Ok(Value::from(vec![
        Value::from(1),
        list![Value::from(4), __1.clone(), __2.clone()],
        Value::from(3),
    ]))
}
fn main() {
    match program() {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
    }
}
