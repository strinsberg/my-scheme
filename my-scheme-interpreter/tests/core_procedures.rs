mod help;

// TODO test different kinds of errors, but not until error types are fully
// setup so that we do not have to retest the messages.

#[test]
fn test_basic_data() {
    help::eval_assert("#\\a", "#\\a");
    help::eval_assert("#\\space", "#\\space");
    help::eval_assert("568", "568");
    help::eval_assert("56.8", "56.8");
    help::eval_assert("\"hello, world!\"", "hello, world!");
    help::eval_assert("\"hello,\\n world!\"", "hello,\n world!");
    help::eval_assert("\"hello,\\t world!\\0\"", "hello,\t world!\0");
    help::eval_assert("#t", "#t");
    help::eval_assert("#f", "#f");
    help::eval_assert("#true", "#t");
    help::eval_assert("#false", "#f");
}

#[test]
fn test_quote() {
    help::eval_assert("'a", "a");
    help::eval_assert("''a", "(quote a)");
    help::eval_assert("  '   '   a  ", "(quote a)");
    help::eval_assert("'#t", "#t");
    help::eval_assert("'(1 2 3 4)", "(1 2 3 4)");
    help::eval_assert("(quote 1 2 3 4)", "1");
    help::eval_assert("(quote (1 2 3 4))", "(1 2 3 4)");
}

#[test]
fn test_if() {
    help::eval_assert("(if #t 'hello 'world)", "hello");
    help::eval_assert("(if #f 'hello 'world)", "world");
    help::eval_assert("(if #t 'no-false)", "no-false");
    help::eval_assert("(if #f 'no-false)", "()");
    help::eval_assert("(if #t 'too 'many 'args)", "too");
    help::eval_assert("(if #f 'too 'many 'args)", "many");
}

#[test]
fn test_basic_arithmetic() {
    // Binary
    help::eval_assert("(+ 4 9)", "13");
    help::eval_assert("(- 4 9)", "-5");
    help::eval_assert("(* 4 9)", "36");
    help::eval_assert("(/ 1 2)", "0.5");
    // Unary
    help::eval_assert("(+ 4)", "4");
    help::eval_assert("(- 4)", "-4");
    help::eval_assert("(* 4)", "4");
    help::eval_assert("(/ 4)", "0.25");
    // Multi
    help::eval_assert("(+ 4 1 2 3)", "10");
    help::eval_assert("(- 4 1 2 3)", "-2");
    help::eval_assert("(* 4 1 2 3)", "24");
    help::eval_assert("(/ 4 1 2 4)", "0.5");
}

#[test]
fn test_basic_lambda() {
    help::eval_assert("((lambda (x) (+ x 1)) 6)", "7");
    help::eval_assert("((lambda (x) (- x 1) (+ x 1)) (- 10 5) (+ 10 5))", "6");
}

#[test]
fn test_define() {
    // Assignments
    help::eval_assert("(define a 5) (+ a 6)", "11");
    help::eval_assert("(define a 5) (define b 6) (+ a b)", "11");
    // Functions
    help::eval_assert("(define f (lambda (x) (+ x 1))) (f 6)", "7");
    help::eval_assert(
        "(define f (lambda (x) (if (eqv? x 0) x (f (- x 1))))) (f 10)",
        "0",
    );
}

#[test]
fn test_let() {
    // Assignments
    help::eval_assert("(let ((a 5)) a)", "5");
    help::eval_assert("(let ((a 5) (b 6)) b)", "6");
    help::eval_assert("(let ((a 5) (b 6)) b a)", "5");
    help::eval_assert("(let ((a 5) (b 6)) (+ b a))", "11");
    // Function
    help::eval_assert("(let ((f (lambda (x) (+ x 1)))) (f 6))", "7");
    // Shadowing
    help::eval_assert(
        "(let ((a 5) (b 6))
           (let ((a 8))
             (+ b a)))",
        "14",
    );
    help::eval_assert(
        "(let ((a 5) (b 6))
           (let ((a 8))
             (+ b a))
           (+ b a))",
        "11",
    );
}

#[test]
fn test_letrec() {
    // Assignments
    help::eval_assert("(letrec ((a 5)) a)", "5");
    help::eval_assert("(letrec ((a 5) (b 6)) b)", "6");
    help::eval_assert("(letrec ((a 5) (b 6)) b a)", "5");
    help::eval_assert("(letrec ((a 5) (b 6)) (+ b a))", "11");
    help::eval_assert("(letrec ((a 5) (b a)) (+ b a))", "10");
    // Functions and recursion
    help::eval_assert("(letrec ((f (lambda (x) (+ x 1)))) (f 6))", "7");
    help::eval_assert(
        "(letrec ((f (lambda (x) (if (eqv? x 0) x (f (- x 1)))))) (f 10))",
        "0",
    );
    help::eval_assert(
        "(letrec ((even? (lambda (x) (if (eqv? x 0) #t (odd? (- x 1)))))
                  (odd? (lambda (x) (if (eqv? x 0) #f (even? (- x 1))))))
           (even? 10))",
        "#t",
    );
    help::eval_assert(
        "(letrec ((even? (lambda (x) (if (eqv? x 0) #t (odd? (- x 1)))))
                  (odd? (lambda (x) (if (eqv? x 0) #f (even? (- x 1))))))
           (even? 51))",
        "#f",
    );
}

#[test]
fn test_set() {
    //help::eval_assert("(define a 5) (set! a 10) (+ a 6)", "16");
    //help::eval_assert("(let ((a 5)) (set! a 10) (+ a 8))", "18");
    help::eval_assert("(letrec ((a 5)) (set! a 10) (+ a 3))", "13");
}

#[test]
fn test_cons_car_cdr() {
    // Cons and dots
    help::eval_assert("(cons 1 3)", "(1 . 3)");
    help::eval_assert("(cons 1 (cons 2 3))", "(1 2 . 3)");
    help::eval_assert("'( 1 . (2 . 3))", "(1 2 . 3)");
    //help::eval_assert("( 1 . (2 . 3))", "(1 2 . 3)");  // Should this work???
    // Car and Cdr
    help::eval_assert("(car '(1 2 3))", "1");
    help::eval_assert("(cdr '(1 2 3))", "(2 3)");
    help::eval_assert("(car (cdr '(1 2 3)))", "2");
    help::eval_assert("(cdr (cdr '(1 2 3)))", "(3)");
    help::eval_assert("(car (cdr (cdr '(1 2 3))))", "3");
    help::eval_assert("(cdr (cdr (cdr '(1 2 3))))", "()");
    help::eval_assert("(cons 5 (cdr (cdr '(1 2 3))))", "(5 3)");
    // Shared prefixes
    help::eval_assert("(define x '(1 2 3)) (cons 5 x)", "(5 1 2 3)");
    help::eval_assert("(define x '(1 2 3)) (cons 5 x) x", "(1 2 3)");
}

#[test]
fn test_closure() {
    help::eval_assert(
        "(define f
           (let ((a 5))
             (lambda (x) (+ x a))))
         (f 6)
         (f 10)",
        "15",
    );
    help::eval_assert(
        "(define f
           (let ((a 5))
             (lambda (x) (set! a (+ a x)) a)))
         (f 1)
         (f 1)
         (f 1)
         (f 3)",
        "11",
    );
    help::eval_assert(
        "(define counter
           (lambda ()
             (let ((a 0))
               (lambda (x) (set! a (+ a x)) a))))
         (define f (counter))
         (define g (counter))
         (f 1)
         (f 1)
         (f 1)
         (g 10)
         (f 1)",
        "4",
    );
    help::eval_assert(
        "(define counter
           (lambda ()
             (let ((a 0))
               (lambda (x) (set! a (+ a x)) a))))
         (define f (counter))
         (define g (counter))
         (f 1)
         (f 1)
         (f 1)
         (g 5)
         (+ (g 1) (f 1))
         ",
        "10",
    );
}

#[test]
fn test_apply() {
    help::eval_assert("(apply + '(1 2))", "3");
    help::eval_assert("(apply (if #t - +) '(1 2))", "-1");
}

#[test]
fn test_eval() {
    help::eval_assert("(eval '(+ 1 2) (null-environment))", "3");
}
