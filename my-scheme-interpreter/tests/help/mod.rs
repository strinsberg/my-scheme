use my_scheme_interpreter::interpreter::Interpreter;

// Testing helpers to make certain tasks easier

// Interpret the code and check that the result string equals the expected string.
pub fn eval_assert(code: &str, expected: &str) {
    let result = Interpreter::new().init().eval_string(code);
    assert_eq!(&result, expected);
}
