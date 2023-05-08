mod help;

#[test]
fn test_cons() {
    help::eval_assert("(cons 'a '())", "(a)");
    help::eval_assert("(cons 'a '(b c))", "(a b c)");
    help::eval_assert("(cons '(a) '(b c))", "((a) b c)");
    help::eval_assert("(cons \"hello\" '(b c))", "(\"hello\" b c)");
    help::eval_assert("(cons '(a b) 'c)", "((a b) . c)");
    help::eval_assert("(cons 1 (cons 2 3))", "(1 2 . 3)");
    help::eval_assert("'( 1 . (2 . 3))", "(1 2 . 3)");
}

#[test]
fn test_car() {
    help::eval_assert("(car '(a b c))", "a");
    help::eval_assert("(car '((a) b c))", "(a)");
    help::eval_assert("(car '(1 . 2))", "1");
    help::eval_bad_arg_error("(car '())", "car", "pair", "()");
}

#[test]
fn test_cdr() {
    // Car and Cdr
    help::eval_assert("(cdr '(1 2 3))", "(2 3)");
    help::eval_assert("(car (cdr '(1 2 3)))", "2");
    help::eval_assert("(cdr (cdr '(1 2 3)))", "(3)");
    help::eval_assert("(car (cdr (cdr '(1 2 3))))", "3");
    help::eval_assert("(cdr (cdr (cdr '(1 2 3))))", "()");
    help::eval_assert("(cons 5 (cdr (cdr '(1 2 3))))", "(5 3)");
}

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

#[test]
fn test_shared_prefixes() {
    // Shared prefixes
    help::eval_assert("(define x '(1 2 3)) (cons 5 x)", "(5 1 2 3)");
    help::eval_assert("(define x '(1 2 3)) (cons 5 x) x", "(1 2 3)");
}
