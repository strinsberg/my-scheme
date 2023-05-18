mod help;

// Because we use the reader to get the expression and build the matching error
// string (so if the error display changes we do not have to change all tests),
// these tests CANNOT check reader/scanner errors. Those should be checked in the
// reader and scanner unit tests.

#[test]
fn test_syntax_errors() {
    help::eval_syntax_error("(1 . 2)");
}

#[test]
fn test_undeclared_symbol_errors() {
    help::eval_undeclared_error("(a 1 2 3)", "a");
}

#[test]
fn test_arity_errors() {
    help::eval_arity_error("(car)", "car", 1);
}

#[test]
fn test_bad_arg_errors() {
    help::eval_bad_arg_error("(car 1)", "car", "pair", "1");
}

#[test]
fn test_bad_arithmetic_errors() {
    help::eval_bad_arithmetic_error("(/ 33 0)", "/", "33", "0");
}

#[test]
fn test_bad_binding_error() {
    help::eval_bad_binding_error("(let ((4 99)) 99)", "4");
}

// User errors
#[test]
fn test_general_user_errors() {
    help::eval_assert(
        "(error! \"func\" \"you made a mistake\" 45 '(1 2 3))",
        "Error in func: you made a mistake\nIrritants: 45 (1 2 3)",
    );
    help::eval_assert(
        "(error! \"func\" \"you made a mistake\")",
        "Error in func: you made a mistake",
    );
    help::eval_bad_arg_error(
        "(arg-type-error! \"func\" 45 \"pair\")",
        "func",
        "pair",
        "45",
    );
}
