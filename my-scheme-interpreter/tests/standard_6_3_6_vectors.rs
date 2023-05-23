mod help;

#[test]
fn test_make_a_vector() {
    help::eval_assert("(vector 1 2 3 4 5)", "#(1 2 3 4 5)");
    help::eval_assert("(make-vector 5)", "#(() () () () ())");
    help::eval_assert("(make-vector 5 'a)", "#(a a a a a)");
    help::eval_assert(
        "(make-vector 'a)",
        "Error in make-vector: a must be exact non-negative integer",
    );
    help::eval_assert(
        "(make-vector)",
        "Error in make-vector: incorrect argument count",
    );
}

#[test]
fn test_vector_length() {
    help::eval_assert("(vector-length '#(1 2 3 4 5))", "5");
    help::eval_assert("(vector-length (vector 1 2 5))", "3");
}

#[test]
fn test_vector_ref() {
    help::eval_assert("(vector-ref '#(1 2 3 4 5) 2)", "3");
    help::eval_assert("(vector-ref (vector 1 2 3 4 5) 4)", "5");
    help::eval_assert(
        "(vector-ref (vector 1 2 3 4 5) 9)",
        "Error in vector-ref: 9 is not a valid index for #(1 2 3 4 5)",
    );
    help::eval_assert(
        "(vector-ref (vector 1 2 3 4 5) -20)",
        "Error in vector-ref: -20 must be exact non-negative integer",
    );
}

#[test]
fn test_vector_set() {
    help::eval_assert(
        "(let ((vec (vector 1 2 3 4 5))) (vector-set! vec 2 '(1 2 3)) vec)",
        "#(1 2 (1 2 3) 4 5)",
    );
}

#[test]
fn test_is_vector() {
    help::eval_assert("(vector? (make-vector 5))", "#t");
    help::eval_assert("(vector? '#(1 2 3 4))", "#t");
    // is not vector
    help::eval_assert("(vector? \"hello, world\")", "#f");
    help::eval_assert("(vector? '())", "#f");
    help::eval_assert("(vector? #t)", "#f");
    help::eval_assert("(vector? #\\G)", "#f");
    help::eval_assert("(vector? (cons 3 4))", "#f");
    help::eval_assert("(vector? '(1 2 3 4))", "#f");
    help::eval_assert("(vector? car)", "#f");
    help::eval_assert("(vector? (lambda (x) a))", "#f");
    help::eval_assert("(vector? 'waldo)", "#f");
    help::eval_assert("(vector? 12)", "#f");
    help::eval_assert("(vector? 10.5)", "#f");
}

#[test]
fn test_vector_to_and_from_list() {
    help::eval_assert("(vector->list (vector 1 2 3 4 5))", "(1 2 3 4 5)");
    help::eval_assert("(vector->list '#(1 #(2 3) 4 5))", "(1 #(2 3) 4 5)");
    help::eval_assert("(list->vector '(1 2 3 4 5))", "#(1 2 3 4 5)");
    help::eval_assert("(list->vector '(1 #(2 3) 4 5))", "#(1 #(2 3) 4 5)");
}

#[test]
fn test_vector_fill() {
    help::eval_assert(
        "(let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'a) vec)",
        "#(a a a a a)",
    );
}
