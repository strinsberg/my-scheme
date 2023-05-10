mod help;

#[test]
fn test_symbols() {
    help::eval_assert("'a", "a");
    help::eval_assert("'Hello-World", "hello-world");
    help::eval_assert("(define a 5) a", "5");
}

#[test]
fn test_is_symbol() {
    help::eval_assert("(symbol? 'hello)", "#t");
    // is not symbol
    help::eval_assert("(symbol? #t)", "#f");
    help::eval_assert("(symbol? \"hello\")", "#f");
    help::eval_assert("(symbol? #\\G)", "#f");
    help::eval_assert("(symbol? (cons 1 4))", "#f");
    help::eval_assert("(symbol? (cons 1 '(4)))", "#f");
    help::eval_assert("(symbol? '#(1 2 3 4))", "#f");
    help::eval_assert("(symbol? car)", "#f");
    help::eval_assert("(symbol? (lambda (x) a))", "#f");
    help::eval_assert("(symbol? 12)", "#f");
    help::eval_assert("(symbol? 10.5)", "#f");
}

#[test]
fn test_symbol_to_string() {
    // our scheme is case insensitive when reading symbols
    help::eval_assert("(symbol->string 'hello-world)", "\"hello-world\"");
    help::eval_assert("(symbol->string 'Hello-World)", "\"hello-world\"");
}

#[test]
fn test_string_to_symbol() {
    help::eval_assert("(string->symbol \"hello-world\")", "hello-world");
    // would be lower-case if read as a symbol
    help::eval_assert("(string->symbol \"Hello-World\")", "Hello-World");
    // has illegal chars for building a symbol
    help::eval_assert(
        "(string->symbol \"-Hello \\n |World|\")",
        "-Hello \n |World|",
    );
}
