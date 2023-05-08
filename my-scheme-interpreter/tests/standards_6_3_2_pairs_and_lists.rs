mod help;

// TODO examples in the standard that do not make sense to me
// (let (f) (list 'non-constant-list) (set-car! (f) 3)) => unsecified
// (let (g) '(constant-list) (set-car! (g) 3)) => error
//

#[test]
fn test_is_pair() {
    help::eval_assert("(pair? (cons 1 2))", "#t");
    help::eval_assert("(pair? (cons 3 '()))", "#t");
    help::eval_assert("(pair? '(1 2 3 4))", "#t");
    // is not pair
    help::eval_assert("(pair? \"hello, world\")", "#f");
    help::eval_assert("(pair? '())", "#f");
    help::eval_assert("(pair? #t)", "#f");
    help::eval_assert("(pair? #\\G)", "#f");
    help::eval_assert("(pair? #(1 2 3 4))", "#f");
    help::eval_assert("(pair? car)", "#f");
    help::eval_assert("(pair? (lambda (x) a))", "#f");
    help::eval_assert("(pair? 'waldo)", "#f");
    help::eval_assert("(pair? 12)", "#f");
    help::eval_assert("(pair? 10.5)", "#f");
}

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
    help::eval_assert("(cdr '((a) b c d))", "(b c d)");
    help::eval_assert("(cdr '(1 . 2))", "2");
    help::eval_bad_arg_error("(cdr '())", "cdr", "pair", "()");
}

#[test]
fn test_set_car() {
    help::eval_assert("(let ((f (list 1 2 3))) (set-car! f 99) f)", "(99 2 3)");
    help::eval_bad_arg_error(
        "(let ((f '(1 2 3))) (set-car! f 99) f)",
        "set-car!",
        "mutable pair",
        "(1 2 3)",
    );
}

#[test]
fn test_set_cdr() {
    help::eval_assert("(let ((f (list 1 2 3))) (set-cdr! f 99) f)", "(1 . 99)");
    help::eval_bad_arg_error(
        "(let ((f '(1 2 3))) (set-cdr! f 99) f)",
        "set-cdr!",
        "mutable pair",
        "(1 2 3)",
    );
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
fn test_is_null() {
    help::eval_assert("(null? '())", "#t");
    // is not null
    help::eval_assert("(null? car)", "#f");
    help::eval_assert("(null? (lambda (x) a))", "#f");
    help::eval_assert("(null? #(1 2 3 4))", "#f");
    help::eval_assert("(null? \"hello, world\")", "#f");
    help::eval_assert("(null? #t)", "#f");
    help::eval_assert("(null? #\\G)", "#f");
    help::eval_assert("(null? (cons 3 4))", "#f");
    help::eval_assert("(null? '(1 2 3 4))", "#f");
    help::eval_assert("(null? 'waldo)", "#f");
    help::eval_assert("(null? 12)", "#f");
    help::eval_assert("(null? 10.5)", "#f");
}

#[test]
fn test_list() {
    help::eval_assert("(list)", "()");
    help::eval_assert("(list 1 (+ 3 4) 2)", "(1 7 2)");
}

#[test]
fn test_is_list() {
    help::eval_assert("(list? (list 1 2 3 4))", "#t");
    help::eval_assert("(list? '(1 2 3 4))", "#t");
    help::eval_assert("(list? '())", "#t");
    // not because dotted and cyclic
    help::eval_assert("(list? '(1 . 2))", "#f");
    help::eval_assert("(let ((x (list 'a))) (set-cdr! x x) (list? x))", "#f");
    // is not list
    help::eval_assert("(null? car)", "#f");
    help::eval_assert("(null? (lambda (x) a))", "#f");
    help::eval_assert("(null? #(1 2 3 4))", "#f");
    help::eval_assert("(null? \"hello, world\")", "#f");
    help::eval_assert("(null? #t)", "#f");
    help::eval_assert("(null? #\\G)", "#f");
    help::eval_assert("(null? (cons 3 4))", "#f");
    help::eval_assert("(null? 'waldo)", "#f");
    help::eval_assert("(null? 12)", "#f");
    help::eval_assert("(null? 10.5)", "#f");
}

// length
// append
// reverse
// list-ref

// others memq, memv, member, assq, assv, assoc

#[test]
fn test_shared_prefixes() {
    help::eval_assert("(define x '(1 2 3)) (cons 5 x)", "(5 1 2 3)");
    help::eval_assert("(define x '(1 2 3)) (cons 5 x) x", "(1 2 3)");
}
