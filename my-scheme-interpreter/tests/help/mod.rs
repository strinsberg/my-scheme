//use my_scheme_interpreter::error::UserError;
use my_scheme_interpreter::interpreter::Interpreter;
use my_scheme_interpreter::reader::StringReader;
use my_scheme_interpreter::value::Value;

// Testing helpers to make certain tasks easier

// Interpret the code and check that the result string equals the expected string.
pub fn eval_assert(code: &str, expected: &str) {
    let result = Interpreter::new().init().eval_string(code);
    assert!(
        &result == expected,
        "\n*** Failed Eval Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

/*
// Error Testing Helpers //

pub fn eval_syntax_error(code: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let expr = StringReader::new(code).read().unwrap();
    let expected = ScmErr::Syntax(expr).to_string();
    assert!(
        result == expected,
        "\n*** Failed Syntax Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_undeclared_error(code: &str, name: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let expected = ScmErr::Undeclared(name.to_string()).to_string();
    assert!(
        result == expected,
        "\n*** Failed Undeclared Symbol Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_arity_error(code: &str, name: &str, arity: usize) {
    let result = Interpreter::new().init().eval_string(code);
    let expected = ScmErr::Arity(name.to_string(), arity).to_string();
    assert!(
        result == expected,
        "\n*** Failed Arity Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_bad_arg_error(code: &str, name: &str, _type: &str, val_str: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let val = StringReader::new(val_str).read().unwrap();
    let expected = ScmErr::BadArgType(name.to_string(), _type.to_string(), val).to_string();
    assert!(
        result == expected,
        "\n*** Failed Argument Type Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_bad_arithmetic_error(code: &str, op: &str, left: &str, right: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let left = StringReader::new(left).read().unwrap();
    let right = StringReader::new(right).read().unwrap();
    let expected = ScmErr::BadArithmetic(op.to_string(), left, right).to_string();
    assert!(
        result == expected,
        "\n*** Failed Arithmetic Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_bad_binding_error(code: &str, binding: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let binding = StringReader::new(binding).read().unwrap();
    let expected = ScmErr::BadBinding(binding).to_string();
    assert!(
        result == expected,
        "\n*** Failed Binding Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_out_of_bounds_error(code: &str, idx: usize, bound: usize) {
    let result = Interpreter::new().init().eval_string(code);
    let expected = ScmErr::OutOfBounds(idx, bound).to_string();
    assert!(
        result == expected,
        "\n*** Failed Out Of Bounds Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}

pub fn eval_range_error(code: &str, name: &str, idx: &str, seq: &str) {
    let result = Interpreter::new().init().eval_string(code);
    let index = StringReader::new(idx).read().unwrap();
    let sequence = StringReader::new(seq).read().unwrap();
    let expected = ScmErr::RangeError(name.to_owned(), index, sequence).to_string();
    assert!(
        result == expected,
        "\n*** Failed Range Error Assert ***\n----\nEvaluated: {}\n----\nActual:    {}\nExpected:  {}\n----\n",
        code,
        result,
        expected
    );
}
*/
