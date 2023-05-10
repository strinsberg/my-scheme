mod help;

#[test]
fn test_string_eval() {
    help::eval_assert("\"with double \\\" quote\"", "\"with double \\\" quote\"");
    help::eval_assert("\"hello, world!\"", "\"hello, world!\"");
    help::eval_assert("\"hello,\\n world!\"", "\"hello,\\n world!\"");
    help::eval_assert("\"hello,\\t world!\\0\"", "\"hello,\\t world!\\0\"");
}

#[test]
fn test_is_string() {
    // TODO add test for mutable strings
    help::eval_assert("(string? \"hello, world\")", "#t");
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
    // TODO Test for mutable strings
    help::eval_assert("(string-length \"hello, world!\")", "13");
    help::eval_assert("(string-length \"hello\")", "5");
}

#[test]
fn test_string_ref() {
    // TODO test for mutable strings
    help::eval_assert("(string-ref \"hello, world!\" 0)", "#\\h");
    help::eval_assert("(string-ref \"hello, world!\" 12)", "#\\!");
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
}

fn test_string_compare() {
    help::eval_assert("(string<? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string<? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string>? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string>? \"hello\" \"Hello\")", "#t");

    help::eval_assert("(string<? \"he\" \"hello\")", "#t");
    help::eval_assert("(string<? \"hello\" \"he\")", "#f");
    help::eval_assert("(string>? \"he\" \"hello\")", "#t");
    help::eval_assert("(string>? \"hello\" \"he\")", "#f");

    help::eval_assert("(string<=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string<=? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string>=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string>=? \"hello\" \"Hello\")", "#t");

    help::eval_assert("(string<=? \"he\" \"hello\")", "#t");
    help::eval_assert("(string<=? \"hello\" \"he\")", "#f");
    help::eval_assert("(string>=? \"he\" \"hello\")", "#t");
    help::eval_assert("(string>=? \"hello\" \"he\")", "#f");
}

fn test_string_compare_ci() {
    // We are a little lax here because the use of a helper for all of them
    // means all we are really checking here is that strings are treated as
    // having equal case.
    help::eval_assert("(string-ci<? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string-ci<? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci>? \"hello\" \"hello\")", "#f");
    help::eval_assert("(string-ci>? \"hello\" \"Hello\")", "#t");

    help::eval_assert("(string-ci<=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci<=? \"Hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci>=? \"hello\" \"hello\")", "#t");
    help::eval_assert("(string-ci>=? \"hello\" \"Hello\")", "#t");
}
