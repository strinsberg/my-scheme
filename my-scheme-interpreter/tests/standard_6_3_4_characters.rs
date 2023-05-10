mod help;

#[test]
fn test_characters() {
    help::eval_assert("#\\a", "#\\a");
    help::eval_assert("#\\H", "#\\H");
    help::eval_assert("#\\*", "#\\*");
    help::eval_assert("#\\space", "#\\space");
}

#[test]
fn test_is_character() {
    help::eval_assert("(char? #\\o)", "#t");
    help::eval_assert("(char? #\\newline)", "#t");
    help::eval_assert("(char? #\\space)", "#t");
    help::eval_assert("(char? #\\null)", "#t");
    help::eval_assert("(char? #\\tab)", "#t");
    // is not char
    help::eval_assert("(char? #t)", "#f");
    help::eval_assert("(char? \"hello\")", "#f");
    help::eval_assert("(char? 'world)", "#f");
    help::eval_assert("(char? (cons 1 4))", "#f");
    help::eval_assert("(char? (cons 1 '(4)))", "#f");
    help::eval_assert("(char? '#(1 2 3 4))", "#f");
    help::eval_assert("(char? car)", "#f");
    help::eval_assert("(char? (lambda (x) a))", "#f");
    help::eval_assert("(char? 12)", "#f");
    help::eval_assert("(char? 10.5)", "#f");
}

#[test]
fn test_integer_to_char() {
    help::eval_assert("(integer->char 48)", "#\\0");
    help::eval_assert("(integer->char 65)", "#\\A");
    help::eval_assert("(integer->char 97)", "#\\a");
    help::eval_assert("(integer->char 0)", "#\\null");
    help::eval_assert("(integer->char 9)", "#\\tab");
    help::eval_assert("(integer->char 10)", "#\\newline");
    help::eval_assert("(integer->char 24)", "#\\unsup");
    help::eval_assert("(integer->char 32)", "#\\space");
}

#[test]
fn test_char_to_integer() {
    help::eval_assert("(char->integer #\\0)", "48");
    help::eval_assert("(char->integer #\\A)", "65");
    help::eval_assert("(char->integer #\\a)", "97");
    help::eval_assert("(char->integer #\\null)", "0");
    help::eval_assert("(char->integer #\\tab)", "9");
    help::eval_assert("(char->integer #\\newline)", "10");
    help::eval_assert("(char->integer #\\unsup)", "24");
    help::eval_assert("(char->integer #\\space)", "32");
}
