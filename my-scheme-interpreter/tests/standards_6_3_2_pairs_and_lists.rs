mod help;

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
    help::eval_assert("(pair? '#(1 2 3 4))", "#f");
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
    //help::eval_bad_arg_error("(car '())", "car", "pair", "()");
}

#[test]
fn test_cdr() {
    help::eval_assert("(cdr '((a) b c d))", "(b c d)");
    help::eval_assert("(cdr '(1 . 2))", "2");
    //help::eval_bad_arg_error("(cdr '())", "cdr", "pair", "()");
}

#[test]
fn test_set_car() {
    // NOTE we do not distinguish between constant and non-constant lists
    help::eval_assert("(let ((f (list 1 2 3))) (set-car! f 99) f)", "(99 2 3)");
    help::eval_assert("(let ((f '(1 2 3))) (set-car! f 99) f)", "(99 2 3)");
    // from standard
    help::eval_assert(
        "(define (f) (list 'non-constant-list)) (set-car! (f) 3)",
        "non-constant-list",
    );
    // Even though the list that is returned is mutated the function creates a new
    // list everytime, so calling f will return an identical list each call.
    // Because I currently have set-car! return the value that was replaced
    // instead of the list, to properly mutate the list's values and access the
    // mutated list the call to f would need to be saved before being mutated.
    help::eval_assert(
        "(define (f) (list 'non-constant-list)) (set-car! (f) 3) (f)",
        "(non-constant-list)",
    );
    // because the list returned is a static list the same list is returned by the
    // function everytime and therfore we change values in the list being returned
    // everytime. This is a violation of the standard which states this should be
    // an error.
    help::eval_assert("(define (f) '(constant-list)) (set-car! (f) 3) (f)", "(3)");
}

#[test]
fn test_set_cdr() {
    help::eval_assert("(let ((f (list 1 2 3))) (set-cdr! f 99) f)", "(1 . 99)");
    //help::eval_bad_arg_error(
    //"(let ((f '(1 2 3))) (set-cdr! f 99) f)",
    //"set-cdr!",
    //"mutable pair",
    //"(1 2 3)",
    //);
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
    help::eval_assert("(null? '#(1 2 3 4))", "#f");
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
    // Note this implementation allows dotted pairs as lists and does not
    // check pairs or lists for cycles.
    help::eval_assert("(list? (list 1 2 3 4))", "#t");
    help::eval_assert("(list? '(1 2 3 4))", "#t");
    help::eval_assert("(list? '())", "#t");
    help::eval_assert("(list? '(1 . 2))", "#t");
    help::eval_assert("(list? '(1 2 3 4 . 5))", "#t");
    help::eval_assert("(list? (cons 3 4))", "#t");
    // is not list
    help::eval_assert("(list? car)", "#f");
    help::eval_assert("(list? (lambda (x) a))", "#f");
    help::eval_assert("(list? '#(1 2 3 4))", "#f");
    help::eval_assert("(list? \"hello, world\")", "#f");
    help::eval_assert("(list? #t)", "#f");
    help::eval_assert("(list? #\\G)", "#f");
    help::eval_assert("(list? 'waldo)", "#f");
    help::eval_assert("(list? 12)", "#f");
    help::eval_assert("(list? 10.5)", "#f");
}

#[test]
fn test_length() {
    help::eval_assert("(length '(1 2 3 4))", "4");
    help::eval_assert("(length (list 1 2 '(a b c) 3 4))", "5");
    //help::eval_bad_arg_error("(length 34)", "length", "proper list", "34");
    //help::eval_bad_arg_error("(length '(1 2 . 3))", "length", "proper list", "(1 2 . 3)");
    //help::eval_bad_arg_error(
    //"(let ((x (list 'a))) (set-cdr! x x) (length x))",
    //"length",
    //"proper list",
    //"(a . #cyclic#)",
    //);
}

#[test]
fn test_append() {
    help::eval_assert("(append '(1 2 3 4) '(5 6 . 7))", "(1 2 3 4 5 6 . 7)");
    help::eval_assert("(append '() 'a)", "a");
    help::eval_assert("(append '(1 (2)) '(3))", "(1 (2) 3)");
    help::eval_assert("(append '(1 2 3 4) '(5 6 7))", "(1 2 3 4 5 6 7)");
    help::eval_assert(
        "(append '(1 2 3 4) '(5 6 7) (list 8 9))",
        "(1 2 3 4 5 6 7 8 9)",
    );
    help::eval_assert(
        "(let ((a '(1 2 3 4)) (b '(5 6 7)) (c (list 8 9))) (append a b c))",
        "(1 2 3 4 5 6 7 8 9)",
    );
    // check that the stored lists are not altered
    help::eval_assert(
        "(let ((a '(1 2 3 4)) (b '(5 6 7)) (c (list 8 9))) (append a b c) a)",
        "(1 2 3 4)",
    );
    help::eval_assert(
        "(let ((a '(1 2 3 4)) (b '(5 6 7)) (c (list 8 9))) (append a b c) b)",
        "(5 6 7)",
    );
    help::eval_assert(
        "(let ((a '(1 2 3 4)) (b '(5 6 7)) (c (list 8 9))) (append a b c) c)",
        "(8 9)",
    );
}

#[test]
fn test_reverse() {
    help::eval_assert("(reverse '(1 2 3 4 5))", "(5 4 3 2 1)");
    help::eval_assert("(reverse '(1 (2 3) 4 (5 (6))))", "((5 (6)) 4 (2 3) 1)");
}

#[test]
fn test_list_ref() {
    help::eval_assert("(list-ref '(1 2 3 4 5) 3)", "4");
    help::eval_assert("(list-ref '(1 (2 3) 4 (5 (6))) 3)", "(5 (6))");
    help::eval_assert("(list-ref '(1 2 3 4 . 5) 3)", "4");
    /*
    help::eval_bad_arg_error(
        "(list-ref '(1 2 3 4 . 5) 4)",
        "list-ref",
        "proper list",
        "(1 2 3 4 . 5)",
    );
    */
    //help::eval_range_error("(list-ref '(1 2 3 4 5) 5)", "list-ref", "5", "(1 2 3 4 5)");
}

#[test]
fn test_list_tail() {
    help::eval_assert("(list-tail '(1 2 3 4 5) 3)", "(4 5)");
    help::eval_assert("(list-tail '(1 (2 3) 4 (5 (6))) 2)", "(4 (5 (6)))");
    help::eval_assert("(list-tail '(1 2 3 4 . 5) 3)", "(4 . 5)");
    help::eval_assert("(list-tail '(1 2 3 4 . 5) 4)", "5");
    help::eval_assert("(list-tail '(1 2 3 ()) 3)", "(())");
    //help::eval_range_error(
    //"(list-tail '(1 2 3 4 5) 5)",
    //"list-tail",
    //"5",
    //"(1 2 3 4 5)",
    //);
}

// others memq, memv, member, assq, assv, assoc

#[test]
fn test_other() {
    // cyclic list display
    //help::eval_assert("(let ((x (list 'a))) (set-cdr! x x) x)", "(a . #cyclic#)");
    // shared prefixes are not affected
    help::eval_assert("(define x '(1 2 3)) (cons 5 x)", "(5 1 2 3)");
    help::eval_assert("(define x '(1 2 3)) (cons 5 x) x", "(1 2 3)");
}
