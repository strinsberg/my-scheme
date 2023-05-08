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
    help::eval_assert("(string? #(1 2 3 4))", "#f");
    help::eval_assert("(string? car)", "#f");
    help::eval_assert("(string? (lambda (x) a))", "#f");
    help::eval_assert("(string? 'waldo)", "#f");
    help::eval_assert("(string? 12)", "#f");
    help::eval_assert("(string? 10.5)", "#f");
}
