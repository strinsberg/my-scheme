use my_scheme_lib::compile::core::*;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
fn program() -> Result<Value, Error> {
    let env = new_env();
    {
        let env = push(&env);
        put(&env, "f", Value::Undefined);
        put(&env, "g", Value::Undefined);
        set(
            &env,
            "f",
            lambda(None, env.clone(), |args, env| {
                put(&env, "x", args[0].clone());
                apply(get(&env, "g")?, vec![get(&env, "x")?])
            }),
        )?;
        set(
            &env,
            "g",
            lambda(None, env.clone(), |args, env| {
                put(&env, "x", args[0].clone());
                apply(get(&env, "+")?, vec![Value::from(1), get(&env, "x")?])
            }),
        )?;
        apply(get(&env, "f")?, vec![Value::from(6)])
    }
}
fn main() {
    match program() {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
    }
}
