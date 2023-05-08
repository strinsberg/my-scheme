mod help;

// Boolean ////////////////////////////////////////////////////////////////////

#[test]
fn test_not() {
    help::eval_assert("(not #t)", "#f");
    help::eval_assert("(not 3)", "#f");
    help::eval_assert("(not '(1 2 3))", "#f");
    help::eval_assert("(not '())", "#f");
    help::eval_assert("(not 'nil)", "#f");
    help::eval_assert("(not #f)", "#t");
}
