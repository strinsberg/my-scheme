mod help;

// TODO test different kinds of errors, but not until error types are fully
// setup so that we do not have to retest the messages.

#[test]
fn test_basic_data() {
    help::eval_assert("#\\a", "#\\a");
    help::eval_assert("#\\space", "#\\space");
    help::eval_assert("568", "568");
    help::eval_assert("56.8", "56.8");
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
    help::eval_assert("(/ 1.0 2)", "0.5");
    help::eval_assert("(/ 1 2)", "1/2");
    // Unary
    help::eval_assert("(+ 4)", "4");
    help::eval_assert("(- 4)", "-4");
    help::eval_assert("(* 4)", "4");
    help::eval_assert("(/ 4.0)", "0.25");
    help::eval_assert("(/ 4)", "1/4");
    // Multi
    help::eval_assert("(+ 4 1 2 3)", "10");
    help::eval_assert("(- 4 1 2 3)", "-2");
    help::eval_assert("(* 4 1 2 3)", "24");
    help::eval_assert("(/ 4.0 1 2 4)", "0.5");
    help::eval_assert("(/ 4 1 2 4)", "1/2");
    help::eval_assert("(/ 4 1 2 -4)", "-1/2");
}

#[test]
#[ignore]
fn test_lambda() {
    help::eval_assert("((lambda (x) (+ x 1)) 6)", "7");
    help::eval_assert("((lambda (x) (- x 1) (+ x 1)) (- 10 5) (+ 10 5))", "6");
    help::eval_assert("((lambda x x) 2 3 4 5)", "(2 3 4 5)");
    help::eval_assert("((lambda (x y . z) z) 2 3 4 5)", "(4 5)");
    // named with define or set!
    help::eval_assert("(lambda (x) (+ x 1))", "#<closure>");
    help::eval_assert("(define f (lambda (x) (+ x 1))) f", "#<procedure f>");
    help::eval_assert("(define (f x) (+ x 1)) f", "#<procedure f>");
    help::eval_assert("(define (f . x) (+ x 1)) f", "#<procedure f>");
    help::eval_assert(
        "(define g '()) (define (f x) (+ x 1)) (set! g f) f",
        "#<procedure f>",
    );
    help::eval_assert(
        "(define g '()) (define (f x) (+ x 1)) (set! g f) g",
        "#<procedure g>",
    );
}

#[test]
#[ignore]
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
    // define lambda helper
    help::eval_assert("(define (f) 13) (f)", "13");
    help::eval_assert("(define (f x) x) (f 6)", "6");
    help::eval_assert("(define (f . x) x) (f 1 2 3 4)", "(1 2 3 4)");
    help::eval_assert("(define (f x y . z) z) (f 1 2 3 4)", "(3 4)");
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
fn test_let_star() {
    // Assignments
    help::eval_assert("(let* ((a 5)) a)", "5");
    help::eval_assert("(let* ((a 5) (b 6)) b)", "6");
    help::eval_assert("(let* ((a 5) (b 6)) b a)", "5");
    help::eval_assert("(let* ((a 5) (b 6)) (+ b a))", "11");
    // Function
    help::eval_assert("(let* ((f (lambda (x) (+ x 1)))) (f 6))", "7");
    // Shadowing
    help::eval_assert(
        "(let* ((a 5) (b 6))
           (let* ((a 8))
             (+ b a)))",
        "14",
    );
    help::eval_assert(
        "(let* ((a 5) (b 6))
           (let* ((a 8))
             (+ b a))
           (+ b a))",
        "11",
    );
    // sequential definitions
    help::eval_assert(
        "(let* ((a 5) (b a))
           (+ b a))",
        "10",
    );
    help::eval_assert(
        "(let* ((a 5) (b 6) (c (+ a b)))
           (+ b a c))",
        "22",
    );
    help::eval_assert(
        "(let* ((a 5) (f (lambda (x) (+ x a))))
           (f 10))",
        "15",
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
    help::eval_assert("(define a 5) (set! a 10) (+ a 6)", "16");
    help::eval_assert("(let ((a 5)) (set! a 10) (+ a 8))", "18");
    help::eval_assert("(letrec ((a 5)) (set! a 10) (+ a 3))", "13");
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

// Type predicates ////////////////////////////////////////////////////////////

#[test]
fn test_is_boolean() {
    help::eval_assert("(boolean? #t)", "#t");
    help::eval_assert("(boolean? #f)", "#t");
    help::eval_assert("(boolean? #true)", "#t");
    help::eval_assert("(boolean? #false)", "#t");
    // is not boolean
    help::eval_assert("(boolean? #\\space)", "#f");
    help::eval_assert("(boolean? \"hello\")", "#f");
    help::eval_assert("(boolean? 'world)", "#f");
    help::eval_assert("(boolean? (cons 1 4))", "#f");
    help::eval_assert("(boolean? (cons 1 '(4)))", "#f");
    help::eval_assert("(boolean? '#(1 2 3 4))", "#f");
    help::eval_assert("(boolean? car)", "#f");
    help::eval_assert("(boolean? (lambda (x) a))", "#f");
    help::eval_assert("(boolean? 12)", "#f");
    help::eval_assert("(boolean? 10.5)", "#f");
}

#[test]
fn test_is_number() {
    help::eval_assert("(number? 0)", "#t");
    help::eval_assert("(number? 1234)", "#t");
    help::eval_assert("(number? -1234)", "#t");
    help::eval_assert("(number? 12.34)", "#t");
    help::eval_assert("(number? -0.1234)", "#t");
    // is not number
    help::eval_assert("(number? #t)", "#f");
    help::eval_assert("(number? \"hello\")", "#f");
    help::eval_assert("(number? #\\G)", "#f");
    help::eval_assert("(number? (cons 1 4))", "#f");
    help::eval_assert("(number? (cons 1 '(4)))", "#f");
    help::eval_assert("(number? '#(1 2 3 4))", "#f");
    help::eval_assert("(number? car)", "#f");
    help::eval_assert("(number? (lambda (x) a))", "#f");
    help::eval_assert("(number? 'waldo)", "#f");
}

// Derived Expressions ////////////////////////////////////////////////////////

#[test]
fn test_and() {
    help::eval_assert("(and)", "#t");
    help::eval_assert("(and (eqv? 2 2) (eqv? 4 4))", "#t");
    help::eval_assert("(and (eqv? 2 3) (eqv? 4 4))", "#f");
    help::eval_assert("(and 1 '(1 2 3) 2 'a '(f g))", "(f g)");
    help::eval_assert("(and (eqv? 2 3) (/ 3 0))", "#f"); // short circuit before error
}

#[test]
fn test_or() {
    help::eval_assert("(or)", "#f");
    help::eval_assert("(or (eqv? 2 2) (eqv? 4 4))", "#t");
    help::eval_assert("(or (eqv? 2 3) (eqv? 4 4))", "#t");
    help::eval_assert("(or #f #f #f)", "#f");
    help::eval_assert("(or '(1 2 3) (/ 3 0))", "(1 2 3)"); // short circuit before error
}

#[test]
fn test_begin() {
    help::eval_assert(
        "(let ((a 5) (b 5)) (begin (set! a 33) (set! b 10) (+ a b)))",
        "43",
    );
}

#[test]
fn test_do() {
    help::eval_assert(
        "(let ((x '(1 3 5 7 9)))
           (do ((x x (cdr x))
                (sum 0 (+ sum (car x))))
              ((null? x) sum)))",
        "25",
    );
    help::eval_assert(
        "(do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
              ((eqv? i 5) vec)
             (vector-set! vec i i))",
        "#(0 1 2 3 4)",
    );
}

#[test]
fn test_cond() {
    help::eval_assert("(cond ((eqv? 2 2) 'first) (else 'second))", "first");
    help::eval_assert(
        "(cond (#f 'first) (#f 'second) (else 'third 'fourth))",
        "fourth",
    );
    help::eval_assert("(cond ('(1 2 3) => car) (else 'second))", "1");
}

#[test]
fn test_case() {
    help::eval_assert(
        "(case (* 2 3) ((1 2 3 4 5) 'first) ((a b c 6) 'second 'third) (else 'fourth))",
        "third",
    );
    help::eval_assert(
        "(case (* 2 3) ((1 2 3 4 5) 'first) ((a b c d) 'second) (else 'third 'fourth))",
        "fourth",
    );
}

#[test]
fn test_eval_mutable_list() {
    help::eval_assert("(eval (list '+ 1 2) (null-environment))", "3");
}

// Other //////////////////////////////////////////////////////////////////////

#[test]
fn test_redefining_keywords() {
    help::eval_assert("(define if 4) if", "4");
    help::eval_assert("(define (if x) (+ x 1)) (if 4)", "5");
}
