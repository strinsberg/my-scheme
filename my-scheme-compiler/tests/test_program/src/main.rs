use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::list;
use my_scheme_lib::proc::help::*;
pub fn module(env: Environment) -> Result<Value, Error> {
    {
        let env = push(&env);
        put(&env, "is-even?", Value::Undefined);
        put(&env, "is-odd?", Value::Undefined);
        set(
            &env,
            "is-even?",
            lambda(None, env.clone(), |args, env| {
                put(&env, "x", args[0].clone());
                if apply(get(&env, "=")?, vec![get(&env, "x")?, Value::from(0)])?.is_true() {
                    Ok(Value::Bool(true))
                } else {
                    apply(
                        get(&env, "is-odd?")?,
                        vec![apply(
                            get(&env, "-")?,
                            vec![get(&env, "x")?, Value::from(1)],
                        )?],
                    )
                }
            })?,
        )?;
        set(
            &env,
            "is-odd?",
            lambda(None, env.clone(), |args, env| {
                put(&env, "x", args[0].clone());
                if apply(get(&env, "=")?, vec![get(&env, "x")?, Value::from(0)])?.is_true() {
                    Ok(Value::Bool(false))
                } else {
                    apply(
                        get(&env, "is-even?")?,
                        vec![apply(
                            get(&env, "-")?,
                            vec![get(&env, "x")?, Value::from(1)],
                        )?],
                    )
                }
            })?,
        )?;
        apply(get(&env, "is-even?")?, vec![Value::from(10000)])
    }
}

fn main() {
    let env = new_env();
    match module(env) {
        Ok(val) => println!("{val}"),
        Err(e) => println!("{:?}", e),
    }
}
