mod help;

#[test]
fn test_string_eval() {
    help::eval_assert("\"with double \\\" quote\"", "\"with double \\\" quote\"");
    help::eval_assert("\"hello, world!\"", "\"hello, world!\"");
    help::eval_assert("\"hello,\\n world!\"", "\"hello,\\n world!\"");
    help::eval_assert("\"hello,\\t world!\\0\"", "\"hello,\\t world!\\0\"");
}

#[test]
fn test_make_string() {
    help::eval_assert("(make-string 5)", "\"\\0\\0\\0\\0\\0\"");
    help::eval_assert("(make-string 5 #\\a)", "\"aaaaa\"");
}

#[test]
fn test_string_set() {
    help::eval_assert(
        "(define str (make-string 5 #\\a)) (string-set! str 2 #\\Q) str",
        "\"aaQaa\"",
    );
}

#[test]
fn test_string() {
    help::eval_assert("(string #\\h #\\e #\\l #\\l #\\o)", "\"hello\"");
}

#[test]
fn test_string_append() {
    help::eval_assert(
        "(string-append \"hello\" \", \" \"world!\")",
        "\"hello, world!\"",
    );
}

#[test]
fn test_string_and_lists() {
    help::eval_assert("(string->list \"hello\")", "(#\\h #\\e #\\l #\\l #\\o)");
    help::eval_assert("(list->string '(#\\h #\\e #\\l #\\l #\\o))", "\"hello\"");
}

#[test]
fn test_substring() {
    help::eval_assert("(substring \"hello\" 0 0)", "\"\"");
    help::eval_assert("(substring \"hello\" 4 4)", "\"\"");
    help::eval_assert("(substring \"hello\" 0 5)", "\"hello\"");
    help::eval_assert("(substring \"hello\" 4 5)", "\"o\"");
}

#[test]
fn test_string_copy() {
    help::eval_assert("(string-copy \"hello\")", "\"hello\"");
    /* TODO This won't work until eq? is proper, but should confirm that these are
     * not the same string.
    help::eval_assert(
        "(define str \"hello\")
                       (define other (string-copy \"hello\"))
                       (eq? str other)",
        "#f",
    );
    */
}

#[test]
fn test_is_string() {
    help::eval_assert("(string? \"hello, world\")", "#t");
    help::eval_assert("(string? (make-string 5))", "#t");
    // is not string
    help::eval_assert("(string? #t)", "#f");
    help::eval_assert("(string? #\\G)", "#f");
    help::eval_assert("(string? (cons 1 4))", "#f");
    help::eval_assert("(string? (cons 1 '(4)))", "#f");
    help::eval_assert("(string? '#(1 2 3 4))", "#f");
    help::eval_assert("(string? car)", "#f");
    help::eval_assert("(string? (lambda (x) a))", "#f");
    help::eval_assert("(string? 'waldo)", "#f");
    help::eval_assert("(string? 12)", "#f");
    help::eval_assert("(string? 10.5)", "#f");
}

#[test]
fn test_string_length() {
    help::eval_assert("(string-length \"hello, world!\")", "13");
    help::eval_assert("(string-length \"hello\")", "5");
    help::eval_assert("(string-length (make-string 99))", "99");
}

#[test]
fn test_string_ref() {
    help::eval_assert("(string-ref \"hello, world!\" 0)", "#\\h");
    help::eval_assert("(string-ref \"hello, world!\" 12)", "#\\!");
    help::eval_assert("(string-ref (make-string 5 #\\%) 3)", "#\\%");
    help::eval_range_error(
        "(string-ref \"hello, world!\" 13)",
        "string-ref",
        "13",
        "\"hello, world!\"",
    );
}

#[test]
fn test_string_equality() {
    help::eval_assert("(string=? \"hello, world!\" \"hello, world!\")", "#t");
    help::eval_assert("(string=? \"hello, World!\" \"hello, world!\")", "#f");
    help::eval_assert("(string=? \"hello, World!\" \"hello\")", "#f");

    help::eval_assert("(string-ci=? \"hello, world!\" \"hello, world!\")", "#t");
    help::eval_assert("(string-ci=? \"hello, World!\" \"hello, world!\")", "#t");
    help::eval_assert("(string-ci=? \"hello, World!\" \"hello\")", "#f");

    help::eval_assert("(string=? (make-string 4 #\\R) (make-string 4 #\\R))", "#t");
    help::eval_assert("(string=? (make-string 4 #\\T) (make-string 4 #\\R))", "#f");
    help::eval_assert(
        "(string=? (make-string 10 #\\R) (make-string 4 #\\R))",
        "#f",
    );
}

#[test]
fn test_string_compare() {
    help::eval_assert("(string<? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string<? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string>? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string>? \"hello\" \"Hello\")", "#t");

    help::eval_assert("(string<? \"he\" \"hello\")", "#t");
    help::eval_assert("(string<? \"hello\" \"he\")", "#f");
    help::eval_assert("(string>? \"he\" \"hello\")", "#f");
    help::eval_assert("(string>? \"hello\" \"he\")", "#t");

    help::eval_assert("(string<=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string<=? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string>=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string>=? \"hello\" \"Hello\")", "#t");

    help::eval_assert("(string<=? \"he\" \"hello\")", "#t");
    help::eval_assert("(string<=? \"hello\" \"he\")", "#f");
    help::eval_assert("(string>=? \"he\" \"hello\")", "#f");
    help::eval_assert("(string>=? \"hello\" \"he\")", "#t");
}

#[test]
fn test_string_compare_ci() {
    // We are a little lax here because the use of a helpers for all of them
    // means all we are really checking here is that strings are treated as
    // having equal case.
    help::eval_assert("(string-ci<? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string-ci<? \"Hello\" \"hello\")", "#f");
    help::eval_assert("(string-ci>? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string-ci>? \"hello\" \"Hello\")", "#f");

    help::eval_assert("(string-ci<=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci<=? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci>=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci>=? \"hello\" \"Hello\")", "#t");
}

#[test]
fn test_string_fill() {
    help::eval_assert(
        "(define V (make-string 5)) (string-fill! V #\\f) V",
        "\"fffff\"",
    );
}
