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
    help::eval_assert("(integer->char 127)", "#\\unsup");
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
    help::eval_assert("(char->integer #\\unsup)", "127");
    help::eval_assert("(char->integer #\\space)", "32");
}

#[test]
fn test_is_alphabetic() {
    // could make a loop and test them all?
    help::eval_assert("(char-alphabetic? #\\A)", "#t");
    help::eval_assert("(char-alphabetic? #\\Z)", "#t");
    help::eval_assert("(char-alphabetic? #\\a)", "#t");
    help::eval_assert("(char-alphabetic? #\\z)", "#t");
    // not
    help::eval_assert("(char-alphabetic? #\\0)", "#f");
    help::eval_assert("(char-alphabetic? #\\9)", "#f");
    help::eval_assert("(char-alphabetic? #\\null)", "#f");
    help::eval_assert("(char-alphabetic? #\\tab)", "#f");
    help::eval_assert("(char-alphabetic? #\\newline)", "#f");
    help::eval_assert("(char-alphabetic? #\\unsup)", "#f");
    help::eval_assert("(char-alphabetic? #\\space)", "#f");
}

#[test]
fn test_is_numeric() {
    help::eval_assert("(char-numeric? #\\0)", "#t");
    help::eval_assert("(char-numeric? #\\1)", "#t");
    help::eval_assert("(char-numeric? #\\2)", "#t");
    help::eval_assert("(char-numeric? #\\3)", "#t");
    help::eval_assert("(char-numeric? #\\4)", "#t");
    help::eval_assert("(char-numeric? #\\5)", "#t");
    help::eval_assert("(char-numeric? #\\6)", "#t");
    help::eval_assert("(char-numeric? #\\7)", "#t");
    help::eval_assert("(char-numeric? #\\8)", "#t");
    help::eval_assert("(char-numeric? #\\9)", "#t");
    // not
    help::eval_assert("(char-numeric? #\\A)", "#f");
    help::eval_assert("(char-numeric? #\\Z)", "#f");
    help::eval_assert("(char-numeric? #\\a)", "#f");
    help::eval_assert("(char-numeric? #\\z)", "#f");
    help::eval_assert("(char-numeric? #\\null)", "#f");
    help::eval_assert("(char-numeric? #\\tab)", "#f");
    help::eval_assert("(char-numeric? #\\newline)", "#f");
    help::eval_assert("(char-numeric? #\\unsup)", "#f");
    help::eval_assert("(char-numeric? #\\space)", "#f");
}

#[test]
fn test_is_whitespace() {
    help::eval_assert("(char-whitespace? #\\tab)", "#t");
    help::eval_assert("(char-whitespace? #\\newline)", "#t");
    help::eval_assert("(char-whitespace? #\\space)", "#t");
    // not
    help::eval_assert("(char-whitespace? #\\unsup)", "#f");
    help::eval_assert("(char-whitespace? #\\null)", "#f");
    help::eval_assert("(char-whitespace? #\\A)", "#f");
    help::eval_assert("(char-whitespace? #\\Z)", "#f");
    help::eval_assert("(char-whitespace? #\\a)", "#f");
    help::eval_assert("(char-whitespace? #\\z)", "#f");
    help::eval_assert("(char-whitespace? #\\0)", "#f");
    help::eval_assert("(char-whitespace? #\\9)", "#f");
}

#[test]
fn test_is_alphanumeric() {
    help::eval_assert("(char-alphanumeric? #\\A)", "#t");
    help::eval_assert("(char-alphanumeric? #\\Z)", "#t");
    help::eval_assert("(char-alphanumeric? #\\a)", "#t");
    help::eval_assert("(char-alphanumeric? #\\z)", "#t");
    help::eval_assert("(char-alphanumeric? #\\0)", "#t");
    help::eval_assert("(char-alphanumeric? #\\9)", "#t");
    // not
    help::eval_assert("(char-alphanumeric? #\\null)", "#f");
    help::eval_assert("(char-alphanumeric? #\\tab)", "#f");
    help::eval_assert("(char-alphanumeric? #\\newline)", "#f");
    help::eval_assert("(char-alphanumeric? #\\unsup)", "#f");
    help::eval_assert("(char-alphanumeric? #\\space)", "#f");
}

#[test]
fn test_is_uppercase() {
    help::eval_assert("(char-upper-case? #\\A)", "#t");
    help::eval_assert("(char-upper-case? #\\Z)", "#t");
    // not
    help::eval_assert("(char-upper-case? #\\a)", "#f");
    help::eval_assert("(char-upper-case? #\\z)", "#f");
    help::eval_assert("(char-upper-case? #\\0)", "#f");
    help::eval_assert("(char-upper-case? #\\9)", "#f");
    help::eval_assert("(char-upper-case? #\\null)", "#f");
    help::eval_assert("(char-upper-case? #\\tab)", "#f");
    help::eval_assert("(char-upper-case? #\\newline)", "#f");
    help::eval_assert("(char-upper-case? #\\space)", "#f");
}

#[test]
fn test_is_lowercase() {
    help::eval_assert("(char-lower-case? #\\a)", "#t");
    help::eval_assert("(char-lower-case? #\\z)", "#t");
    // not
    help::eval_assert("(char-lower-case? #\\A)", "#f");
    help::eval_assert("(char-lower-case? #\\Z)", "#f");
    help::eval_assert("(char-lower-case? #\\0)", "#f");
    help::eval_assert("(char-lower-case? #\\9)", "#f");
    help::eval_assert("(char-lower-case? #\\null)", "#f");
    help::eval_assert("(char-lower-case? #\\tab)", "#f");
    help::eval_assert("(char-lower-case? #\\newline)", "#f");
    help::eval_assert("(char-lower-case? #\\space)", "#f");
}

#[test]
fn test_is_unsup() {
    help::eval_assert("(char-unsup? #\\unsup)", "#t");
    // not
    help::eval_assert("(char-unsup? #\\A)", "#f");
    help::eval_assert("(char-unsup? #\\Z)", "#f");
    help::eval_assert("(char-unsup? #\\a)", "#f");
    help::eval_assert("(char-unsup? #\\z)", "#f");
    help::eval_assert("(char-unsup? #\\0)", "#f");
    help::eval_assert("(char-unsup? #\\9)", "#f");
    help::eval_assert("(char-unsup? #\\null)", "#f");
    help::eval_assert("(char-unsup? #\\tab)", "#f");
    help::eval_assert("(char-unsup? #\\newline)", "#f");
    help::eval_assert("(char-unsup? #\\space)", "#f");
}

#[test]
fn test_up_case() {
    help::eval_assert("(char-upcase #\\a)", "#\\A");
    help::eval_assert("(char-upcase #\\z)", "#\\Z");
    help::eval_assert("(char-upcase #\\A)", "#\\A");
    help::eval_assert("(char-upcase #\\Z)", "#\\Z");
    help::eval_assert("(char-upcase #\\0)", "#\\0");
    help::eval_assert("(char-upcase #\\9)", "#\\9");
    help::eval_assert("(char-upcase #\\%)", "#\\%");
    help::eval_assert("(char-upcase #\\null)", "#\\null");
    help::eval_assert("(char-upcase #\\tab)", "#\\tab");
    help::eval_assert("(char-upcase #\\newline)", "#\\newline");
    help::eval_assert("(char-upcase #\\unsup)", "#\\unsup");
    help::eval_assert("(char-upcase #\\space)", "#\\space");
}

#[test]
fn test_down_case() {
    help::eval_assert("(char-downcase #\\a)", "#\\a");
    help::eval_assert("(char-downcase #\\z)", "#\\z");
    help::eval_assert("(char-downcase #\\A)", "#\\a");
    help::eval_assert("(char-downcase #\\Z)", "#\\z");
    help::eval_assert("(char-downcase #\\0)", "#\\0");
    help::eval_assert("(char-downcase #\\9)", "#\\9");
    help::eval_assert("(char-downcase #\\%)", "#\\%");
    help::eval_assert("(char-downcase #\\null)", "#\\null");
    help::eval_assert("(char-downcase #\\tab)", "#\\tab");
    help::eval_assert("(char-downcase #\\newline)", "#\\newline");
    help::eval_assert("(char-downcase #\\unsup)", "#\\unsup");
    help::eval_assert("(char-downcase #\\space)", "#\\space");
}

#[test]
fn test_char_eq() {
    help::eval_assert("(char=? #\\a #\\a)", "#t");
    help::eval_assert("(char=? #\\A #\\a)", "#f");
    help::eval_assert("(char=? #\\% #\\#)", "#f");
    help::eval_assert("(char=? #\\space #\\space)", "#t");
    help::eval_assert("(char=? #\\null #\\space)", "#f");
}

fn test_char_less() {
    help::eval_assert("(char<? #\\a #\\a)", "#f");
    help::eval_assert("(char<? #\\e #\\t)", "#t");
    help::eval_assert("(char<? #\\o #\\k)", "#f");
    help::eval_assert("(char<? #\\A #\\a)", "#t");
    help::eval_assert("(char<? #\\A #\\A)", "#f");
    help::eval_assert("(char<? #\\E #\\T)", "#t");
    help::eval_assert("(char<? #\\O #\\K)", "#f");
    help::eval_assert("(char<? #\\% #\\#)", "#f");
    help::eval_assert("(char<? #\\space #\\!)", "#t");
    help::eval_assert("(char<? #\\null #\\space)", "#t");
}

fn test_char_less_eq() {
    help::eval_assert("(char<=? #\\a #\\a)", "#t");
    help::eval_assert("(char<=? #\\e #\\t)", "#t");
    help::eval_assert("(char<=? #\\o #\\k)", "#f");
    help::eval_assert("(char<=? #\\A #\\a)", "#t");
    help::eval_assert("(char<=? #\\A #\\A)", "#t");
    help::eval_assert("(char<=? #\\E #\\T)", "#t");
    help::eval_assert("(char<=? #\\O #\\K)", "#f");
    help::eval_assert("(char<=? #\\% #\\#)", "#f");
    help::eval_assert("(char<=? #\\space #\\!)", "#t");
    help::eval_assert("(char<=? #\\null #\\space)", "#t");
}

fn test_char_greater() {
    help::eval_assert("(char>? #\\a #\\a)", "#f");
    help::eval_assert("(char>? #\\e #\\t)", "#f");
    help::eval_assert("(char>? #\\o #\\k)", "#t");
    help::eval_assert("(char>? #\\A #\\a)", "#f");
    help::eval_assert("(char>? #\\A #\\A)", "#f");
    help::eval_assert("(char>? #\\E #\\T)", "#f");
    help::eval_assert("(char>? #\\O #\\K)", "#t");
    help::eval_assert("(char>? #\\% #\\#)", "#t");
    help::eval_assert("(char>? #\\space #\\!)", "#f");
    help::eval_assert("(char>? #\\null #\\space)", "#f");
}

fn test_char_greater_eq() {
    help::eval_assert("(char>=? #\\a #\\a)", "#t");
    help::eval_assert("(char>=? #\\e #\\t)", "#f");
    help::eval_assert("(char>=? #\\o #\\k)", "#t");
    help::eval_assert("(char>=? #\\A #\\a)", "#t");
    help::eval_assert("(char>=? #\\A #\\A)", "#f");
    help::eval_assert("(char>=? #\\E #\\T)", "#f");
    help::eval_assert("(char>=? #\\O #\\K)", "#t");
    help::eval_assert("(char>=? #\\% #\\#)", "#t");
    help::eval_assert("(char>=? #\\space #\\!)", "#f");
    help::eval_assert("(char>=? #\\null #\\space)", "#f");
}

fn test_char_comp_ci() {
    help::eval_assert("(char-ci=? #\\a #\\a)", "#t");
    help::eval_assert("(char-ci=? #\\A #\\a)", "#t");

    help::eval_assert("(char-ci>? #\\a #\\a)", "#f");
    help::eval_assert("(char-ci>? #\\A #\\a)", "#f");
    help::eval_assert("(char-ci>? #\\a #\\H)", "#f");
    help::eval_assert("(char-ci>? #\\H #\\a)", "#t");

    help::eval_assert("(char-ci<? #\\a #\\a)", "#f");
    help::eval_assert("(char-ci<? #\\A #\\a)", "#f");
    help::eval_assert("(char-ci<? #\\a #\\H)", "#t");
    help::eval_assert("(char-ci<? #\\H #\\a)", "#f");

    help::eval_assert("(char-ci>=? #\\a #\\a)", "#t");
    help::eval_assert("(char-ci>=? #\\A #\\a)", "#t");
    help::eval_assert("(char-ci>=? #\\a #\\H)", "#f");
    help::eval_assert("(char-ci>=? #\\H #\\a)", "#t");

    help::eval_assert("(char-ci<=? #\\a #\\a)", "#t");
    help::eval_assert("(char-ci<=? #\\A #\\a)", "#t");
    help::eval_assert("(char-ci<=? #\\a #\\H)", "#t");
    help::eval_assert("(char-ci<=? #\\H #\\a)", "#f");
}
