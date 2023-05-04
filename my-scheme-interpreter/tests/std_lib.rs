mod help;

#[test]
fn test_extra_car_cdr_variants() {
    help::eval_assert("(caar '((((1) 2) 3) 4))", "((1) 2)");
    help::eval_assert("(caaar '((((1) 2) 3) 4))", "(1)");
    help::eval_assert("(caaaar '((((1) 2) 3) 4))", "1");

    help::eval_assert("(cddr '(1 2 3 4 5 6 7 8 9))", "(3 4 5 6 7 8 9)");
    help::eval_assert("(cdddr '(1 2 3 4 5 6 7 8 9))", "(4 5 6 7 8 9)");
    help::eval_assert("(cddddr '(1 2 3 4 5 6 7 8 9))", "(5 6 7 8 9)");

    help::eval_assert("(cadr '(1 ((9 8) 7) (2) 3 4))", "((9 8) 7)");
    help::eval_assert("(caadr '(1 ((9 8) 7) (2) 3 4))", "(9 8)");
    help::eval_assert("(caaadr '(1 ((9 8) 7) (2) 3 4))", "9");

    help::eval_assert("(caddr '(1 (9 8 7) (2) 3 4))", "(2)");
    help::eval_assert("(cadddr '(1 (9 (8 7)) (2) 3 4))", "3");

    help::eval_assert("(caaddr '(1 (9 (8 7)) (2) 3 4))", "2");
}
