use my_scheme_interpreter::interpreter::Interpreter;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn interpret_my_scheme(code: &str) -> String {
    // Note that this evaluates all code in a new context everytime.
    // The value is that if there is a panic/abort any new evaluation will
    // not be affected by a bad program state. However, this currently will not
    // allow for something like a repl. If startup time is long it will also
    // cause a delay when evaluating.
    Interpreter::new().init().eval_string(code)
}
