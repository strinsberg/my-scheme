mod help;

#[test]
fn test_is_procedure() {
    help::eval_assert("(procedure? car)", "#t");
    help::eval_assert("(procedure? (lambda (x) a))", "#t");
    // is not procedure
    help::eval_assert("(procedure? '#(1 2 3 4))", "#f");
    help::eval_assert("(procedure? \"hello, world\")", "#f");
    help::eval_assert("(procedure? '())", "#f");
    help::eval_assert("(procedure? #t)", "#f");
    help::eval_assert("(procedure? #\\G)", "#f");
    help::eval_assert("(procedure? (cons 3 4))", "#f");
    help::eval_assert("(procedure? '(1 2 3 4))", "#f");
    help::eval_assert("(procedure? 'waldo)", "#f");
    help::eval_assert("(procedure? 12)", "#f");
    help::eval_assert("(procedure? 10.5)", "#f");
}

#[test]
fn test_map() {
    help::eval_assert("(map car '((1 2 3) (4 5 6) (7 8 9)))", "(1 4 7)");
    help::eval_assert("(map + '(1 2 3 4) '(1 2 3 4))", "(2 4 6 8)");
    help::eval_assert(
        "(map (lambda (x y z) (+ x y z)) '(1 2 3 4) '(1 2 3 4) '(1 2 3 4))",
        "(3 6 9 12)",
    );
}
