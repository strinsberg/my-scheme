use crate::char::Char;
use crate::err::ScanError;
use crate::number::Num;
use crate::rep::DisplayRep;
use crate::string::Str;
use std::fmt;

// Tokens /////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(Str),
    Boolean(bool),
    Number(Num),
    Character(Char),
    String(Str),
    LParen,
    RParen,
    VecOpen,
    Quote,
    BackTick,
    Comma,
    CommaAt,
    Dot,
    EOF,
    Cyclic,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{}", s.to_display()),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::Number(n) => write!(f, "{}", n.to_string()),
            Token::Character(c) => write!(f, "{}", c.to_display()),
            Token::String(s) => write!(f, "{}", s.to_display()),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::VecOpen => write!(f, "#("),
            Token::Quote => write!(f, "'"),
            Token::BackTick => write!(f, "`"),
            Token::Comma => write!(f, ","),
            Token::CommaAt => write!(f, ",@"),
            Token::Dot => write!(f, "."),
            Token::EOF => write!(f, "EOF"),
            Token::Cyclic => write!(f, "#cyclic#"),
        }
    }
}

// TODO most of the tokens that include multiple user supplied characters are
// supposed to end only on delimeter and EOF. I am not entirely sure if they do
// as expected as they are mostly checked with spaces between tokens.
//
// Scanner ////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Scanner {
    pub line: usize,
    idx: usize,
    text: Vec<u8>,
}

impl Scanner {
    pub fn new(text: &str) -> Scanner {
        Scanner {
            line: 1,
            idx: 0,
            text: text.into(),
        }
    }

    pub fn next(&mut self) -> Result<Token, ScanError> {
        self.skip_whitespace();
        if self.eof() {
            return Ok(Token::EOF);
        }

        let byte = self.next_byte();
        let ch = byte as char;

        match ch {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '\'' => Ok(Token::Quote),
            '`' => Ok(Token::BackTick),
            ';' => {
                self.skip_comment();
                self.next()
            }
            '#' => self.scan_hash(),
            ',' => self.scan_comma(),
            '"' => self.scan_string(),
            '+' | '-' | '.' => self.scan_peculiar_identifier(byte),
            _ => {
                if is_digit(byte) {
                    self.scan_number(byte)
                } else if is_initial(byte) {
                    self.scan_identifier(byte)
                } else {
                    Err(ScanError::BadChar(self.line, byte as char))
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.eof() {
            let byte = self.peek_byte();
            if !is_whitespace(byte) {
                break;
            } else if byte as char == '\n' {
                self.line += 1;
            }
            self.next_byte();
        }
    }

    fn skip_comment(&mut self) {
        while !self.eof() && self.next_byte() as char != '\n' {}
    }

    // Token Parsers //

    fn scan_hash(&mut self) -> Result<Token, ScanError> {
        let byte = self.next_byte();
        let ch = byte as char;

        match ch {
            '\\' => self.scan_char(),
            '(' => Ok(Token::VecOpen),
            't' | 'f' | 'T' | 'F' => self.scan_bool(byte),
            // TODO b d o x indicate number
            'c' => self.scan_cyclic(),
            _ => Err(ScanError::BadChar(self.line, ch)),
        }
    }

    fn scan_char(&mut self) -> Result<Token, ScanError> {
        let byte = self.next_byte();

        // If it is a named char get the name and create the ascii byte
        if is_letter(byte) && is_letter(self.peek_byte()) {
            let bytes = self.scan_bytes_lower(byte);
            match as_string(&bytes).as_str() {
                "newline" => Ok(Token::Character(Char::LineFeed)),
                "null" => Ok(Token::Character(Char::Null)),
                "space" => Ok(Token::Character(Char::Space)),
                "tab" => Ok(Token::Character(Char::Tab)),
                "unsup" => Ok(Token::Character(Char::Unsupported)),
                name => Err(ScanError::BadIdentifier(self.line, format!("#\\{}", name))),
            }

        // Else treat it as a single byte character and create the ascii byte
        } else {
            self.new_ascii_char(byte)
        }
    }

    fn scan_bool(&mut self, byte: u8) -> Result<Token, ScanError> {
        let bytes = self.scan_bytes_lower(byte);
        match as_string(&bytes).as_str() {
            "t" | "true" => Ok(Token::Boolean(true)),
            "f" | "false" => Ok(Token::Boolean(false)),
            name => Err(ScanError::BadIdentifier(self.line, format!("#{}", name))),
        }
    }

    fn scan_cyclic(&mut self) -> Result<Token, ScanError> {
        let bytes = self.scan_bytes('c' as u8);
        match as_string(&bytes).as_str() {
            "cyclic#" => Ok(Token::Cyclic),
            ident => Err(ScanError::BadIdentifier(self.line, format!("#{}", ident))),
        }
    }

    fn scan_comma(&mut self) -> Result<Token, ScanError> {
        if self.peek_byte() as char == '@' {
            self.next_byte();
            Ok(Token::CommaAt)
        } else {
            Ok(Token::Comma)
        }
    }

    fn scan_string(&mut self) -> Result<Token, ScanError> {
        let mut bytes = Vec::new();

        loop {
            if self.eof() {
                return Err(ScanError::Eof(self.line));
            }

            let byte = self.next_byte();
            match byte as char {
                '"' => break,
                '\\' => match self.next_byte() as char {
                    '0' => bytes.push('\0' as u8),
                    'n' => bytes.push('\n' as u8),
                    't' => bytes.push('\t' as u8),
                    '\\' => bytes.push('\\' as u8),
                    '"' => bytes.push('"' as u8),
                    b => return Err(ScanError::BadEscape(self.line, format!("\\{}", b))),
                },
                '\n' => return Err(ScanError::MultiLineString(self.line)),
                _ => bytes.push(byte),
            }
        }

        Ok(Token::String(Str::from(&bytes[..])))
    }

    fn scan_number(&mut self, byte: u8) -> Result<Token, ScanError> {
        let bytes = self.scan_bytes_lower(byte);
        self.new_number(&bytes)
    }

    fn scan_peculiar_identifier(&mut self, byte: u8) -> Result<Token, ScanError> {
        match byte as char {
            '+' | '-' => match self.peek_byte() as char {
                'i' | '0'..='9' => self.scan_number(byte),
                next if is_delimeter(next as u8) => self.new_identifier(&vec![byte]),
                ch => Err(ScanError::BadChar(self.line, ch)),
            },
            '.' => match self.peek_byte() as char {
                '.' => self.scan_dots(),
                next if is_delimeter(next as u8) || self.eof() => Ok(Token::Dot),
                ch => Err(ScanError::BadChar(self.line, ch)),
            },
            ch => panic!("should be one of [+, -, .]: {ch}"),
        }
    }

    fn scan_dots(&mut self) -> Result<Token, ScanError> {
        let bytes = self.scan_bytes('.' as u8);
        match as_string(&bytes).as_str() {
            "..." => self.new_identifier(&bytes),
            ident => Err(ScanError::BadIdentifier(self.line, ident.to_owned())),
        }
    }

    fn scan_identifier(&mut self, byte: u8) -> Result<Token, ScanError> {
        let bytes = self.scan_bytes_lower(byte);
        if bytes.iter().all(|b| is_subsequent(*b)) {
            self.new_identifier(&bytes)
        } else {
            Err(ScanError::BadIdentifier(self.line, as_string(&bytes)))
        }
    }

    // Byte collection helpers //

    pub fn eof(&self) -> bool {
        self.idx >= self.text.len()
    }

    fn next_byte(&mut self) -> u8 {
        if self.eof() {
            0
        } else {
            let idx = self.idx;
            self.idx += 1;
            self.text[idx]
        }
    }

    fn peek_byte(&self) -> u8 {
        if self.eof() {
            0
        } else {
            self.text[self.idx]
        }
    }

    // Scan bytes until reaching a delimeter char
    fn scan_bytes(&mut self, start: u8) -> Vec<u8> {
        let mut bytes = vec![start];
        while !self.eof() {
            let byte = self.peek_byte();
            if is_delimeter(byte) {
                break;
            } else {
                bytes.push(byte);
                self.next_byte();
            }
        }
        bytes
    }

    // Scan bytes until reaching a delimeter char LOWER CASE them all
    fn scan_bytes_lower(&mut self, start: u8) -> Vec<u8> {
        let mut bytes = vec![lower_case(start)];
        while !self.eof() {
            let byte = self.peek_byte();
            if is_delimeter(byte) {
                break;
            } else {
                bytes.push(lower_case(byte));
                self.next_byte();
            }
        }
        bytes
    }

    // Token Helpers //

    fn new_ascii_char(&self, byte: u8) -> Result<Token, ScanError> {
        match Char::from(byte) {
            Char::Unsupported => Err(ScanError::BadChar(self.line, byte as char)),
            ascii => Ok(Token::Character(ascii)),
        }
    }

    fn new_identifier(&self, bytes: &Vec<u8>) -> Result<Token, ScanError> {
        Ok(Token::Identifier(Str::from(&bytes[..])))
    }

    fn new_number(&self, bytes: &Vec<u8>) -> Result<Token, ScanError> {
        let s: String = bytes.iter().map(|b| *b as char).collect();
        match s.parse::<Num>() {
            Ok(n) => Ok(Token::Number(n)),
            Err(_) => Err(ScanError::BadNumber(self.line, s)), // TODO change to ArithErr when ready
        }
    }
}

// Predicates /////////////////////////////////////////////////////////////////

pub fn is_whitespace(ch: u8) -> bool {
    match ch as char {
        ' ' | '\t' | '\n' => true,
        _ => false,
    }
}

pub fn is_delimeter(ch: u8) -> bool {
    is_whitespace(ch)
        || match ch as char {
            '(' | ')' | '"' | ';' => true,
            _ => false,
        }
}

pub fn is_initial(ch: u8) -> bool {
    is_letter(ch) || is_special_initial(ch)
}

pub fn is_letter(ch: u8) -> bool {
    match ch as char {
        'a'..='z' | 'A'..='Z' => true,
        _ => false,
    }
}

pub fn is_special_initial(ch: u8) -> bool {
    match ch as char {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        _ => false,
    }
}

pub fn is_subsequent(ch: u8) -> bool {
    is_initial(ch) || is_digit(ch) || is_special_subsequent(ch)
}

pub fn is_digit(ch: u8) -> bool {
    match ch as char {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
        _ => false,
    }
}

pub fn is_hex_digit(ch: u8) -> bool {
    is_digit(ch)
        || match ch as char {
            'a'..='f' | 'A'..='F' => true,
            _ => false,
        }
}

pub fn is_special_subsequent(ch: u8) -> bool {
    match ch as char {
        '+' | '-' | '.' | '@' => true,
        _ => false,
    }
}

// Helpers ////////////////////////////////////////////////////////////////////

fn as_string(bytes: &Vec<u8>) -> String {
    bytes.into_iter().map(|b| *b as char).collect()
}

fn lower_case(byte: u8) -> u8 {
    if byte.is_ascii() {
        byte.to_ascii_lowercase()
    } else {
        byte
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    // General token symbols //

    #[test]
    fn test_scanning_simple_symbols() {
        let mut s = Scanner::new("()',,@#(`.");
        assert_eq!(s.next(), Ok(Token::LParen));
        assert_eq!(s.next(), Ok(Token::RParen));
        assert_eq!(s.next(), Ok(Token::Quote));
        assert_eq!(s.next(), Ok(Token::Comma));
        assert_eq!(s.next(), Ok(Token::CommaAt));
        assert_eq!(s.next(), Ok(Token::VecOpen));
        assert_eq!(s.next(), Ok(Token::BackTick));
        assert_eq!(s.next(), Ok(Token::Dot));
        assert_eq!(s.next(), Ok(Token::EOF));
    }

    // Tokens that start with # (excluding numbers) //

    #[test]
    fn test_scanning_single_chars() {
        let bytes = (33..127).collect::<Vec<u8>>();
        for b in bytes.iter() {
            let mut s = Scanner::new(&format!("#\\{}", *b as char));
            assert_eq!(s.next(), Ok(Token::Character(Char::from(*b))));
        }
    }

    #[test]
    fn test_scanning_named_chars() {
        let mut s = Scanner::new("#\\space #\\tab #\\NEWLINE #\\null");
        assert_eq!(s.next(), Ok(Token::Character(Char::Space)));
        assert_eq!(s.next(), Ok(Token::Character(Char::Tab)));
        assert_eq!(s.next(), Ok(Token::Character(Char::LineFeed)));
        assert_eq!(s.next(), Ok(Token::Character(Char::Null)));
    }

    #[test]
    fn test_scanning_invalid_chars() {
        let mut s = Scanner::new("#\\jERsey");
        assert_eq!(
            s.next(),
            Err(ScanError::BadIdentifier(1, "#\\jersey".to_owned()))
        );

        let mut s = Scanner::new("#\\\x02");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, '\x02')));
    }

    #[test]
    fn test_scanning_bools() {
        let mut s = Scanner::new("#t #f #TRUE #False #flase");
        assert_eq!(s.next(), Ok(Token::Boolean(true)));
        assert_eq!(s.next(), Ok(Token::Boolean(false)));
        assert_eq!(s.next(), Ok(Token::Boolean(true)));
        assert_eq!(s.next(), Ok(Token::Boolean(false)));
        assert_eq!(
            s.next(),
            Err(ScanError::BadIdentifier(1, "#flase".to_owned()))
        );
    }

    #[test]
    fn test_scanning_invalid_hash_identifier() {
        let mut s = Scanner::new("#jeff");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, 'j')));

        let mut s = Scanner::new("#1234");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, '1')));
    }

    // Identifiers //

    #[test]
    fn test_scanning_identifiers() {
        let mut s = Scanner::new("hello WoRlD j345.@+- <>!@%&^*_-+=:?~/$0123456789");
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("hello"))));
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("world"))));
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("j345.@+-"))));
        assert_eq!(
            s.next(),
            Ok(Token::Identifier(Str::from("<>!@%&^*_-+=:?~/$0123456789")))
        );
    }

    #[test]
    fn test_scanning_invalid_identifiers() {
        assert_eq!(
            Scanner::new("@hello").next(),
            Err(ScanError::BadChar(1, '@'))
        );
        assert_eq!(
            Scanner::new("\\hello").next(),
            Err(ScanError::BadChar(1, '\\'))
        );
        assert_eq!(
            Scanner::new("hel`lo").next(),
            Err(ScanError::BadIdentifier(1, "hel`lo".to_owned()))
        );
        assert_eq!(
            Scanner::new("hel,lo").next(),
            Err(ScanError::BadIdentifier(1, "hel,lo".to_owned()))
        );
        assert_eq!(
            Scanner::new("hel'lo").next(),
            Err(ScanError::BadIdentifier(1, "hel'lo".to_owned()))
        );
        assert_eq!(
            Scanner::new("hel#lo").next(),
            Err(ScanError::BadIdentifier(1, "hel#lo".to_owned()))
        );
        assert_eq!(
            Scanner::new("hel\\lo").next(),
            Err(ScanError::BadIdentifier(1, "hel\\lo".to_owned()))
        );
    }

    #[test]
    fn test_scanning_peculiar_identifiers() {
        let mut s = Scanner::new("+ - ...");
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("+"))));
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("-"))));
        assert_eq!(s.next(), Ok(Token::Identifier(Str::from("..."))));
    }

    #[test]
    fn test_scanning_invalid_peculiar_identifiers() {
        let mut s = Scanner::new("+hello");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, 'h')));

        let mut s = Scanner::new("-hello");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, 'h')));

        let mut s = Scanner::new(".hello");
        assert_eq!(s.next(), Err(ScanError::BadChar(1, 'h')));

        let mut s = Scanner::new("....");
        assert_eq!(
            s.next(),
            Err(ScanError::BadIdentifier(1, "....".to_owned()))
        );
    }

    // Numbers //
    //
    // Currently numbers only allow i64 and f64
    // - Through rusts parsers we support floats with exponents, but only
    // with the e/E exponent markers, not s f d l.
    // - No exactness is supported. Integers are exact, Floats are not.
    // - No Binary/Octal/Hexidecimal numbers are supported even if they might parse.
    //
    // Also, some of these tests could really move to the number struct as the scanner
    // really just says if there is a +/- or a digit then scan bytes and send them
    // to the Num constructor for a new number.

    #[test]
    fn scanning_simple_integers() {
        assert_eq!(Scanner::new("100").next(), Ok(Token::Number(Num::Int(100))));
        assert_eq!(
            Scanner::new("+100").next(),
            Ok(Token::Number(Num::Int(100)))
        );
        assert_eq!(Scanner::new("-42").next(), Ok(Token::Number(Num::Int(-42))));
        assert_eq!(Scanner::new("0").next(), Ok(Token::Number(Num::Int(0))));
        assert_eq!(Scanner::new("+0").next(), Ok(Token::Number(Num::Int(0))));
        assert_eq!(Scanner::new("-0").next(), Ok(Token::Number(Num::Int(0))));
        assert_eq!(
            Scanner::new("9223372036854775807").next(),
            Ok(Token::Number(Num::Int(i64::MAX)))
        );
        assert_eq!(
            Scanner::new("+9223372036854775807").next(),
            Ok(Token::Number(Num::Int(i64::MAX)))
        );
        assert_eq!(
            Scanner::new("-9223372036854775808").next(),
            Ok(Token::Number(Num::Int(i64::MIN)))
        );
    }

    #[test]
    fn test_scanning_simple_float() {
        assert_eq!(
            Scanner::new("1.234").next(),
            Ok(Token::Number(Num::Flt(1.234)))
        );
        assert_eq!(
            Scanner::new("+1.234").next(),
            Ok(Token::Number(Num::Flt(1.234)))
        );
        assert_eq!(
            Scanner::new("-1.234").next(),
            Ok(Token::Number(Num::Flt(-1.234)))
        );
        assert_eq!(Scanner::new("0.0").next(), Ok(Token::Number(Num::Flt(0.0))));
        assert_eq!(
            Scanner::new("+0.0").next(),
            Ok(Token::Number(Num::Flt(0.0)))
        );
        assert_eq!(
            Scanner::new("-0.0").next(),
            Ok(Token::Number(Num::Flt(0.0)))
        );
        assert_eq!(
            Scanner::new("-1.7976931348623157e308").next(),
            Ok(Token::Number(Num::Flt(f64::MIN)))
        );
        assert_eq!(
            Scanner::new("2.2250738585072014E-308").next(),
            Ok(Token::Number(Num::Flt(f64::MIN_POSITIVE)))
        );
        assert_eq!(
            Scanner::new("1.7976931348623157E308").next(),
            Ok(Token::Number(Num::Flt(f64::MAX)))
        );
        assert_eq!(
            Scanner::new("-1.8E308").next(),
            Ok(Token::Number(Num::Flt(f64::NEG_INFINITY)))
        );
        assert_eq!(
            Scanner::new("2.5E308").next(),
            Ok(Token::Number(Num::Flt(f64::INFINITY)))
        );
    }

    #[test]
    fn parse_invalid_numbers() {
        assert_eq!(
            Scanner::new("123jkl").next(),
            Err(ScanError::BadNumber(1, "123jkl".to_owned()))
        );
        assert_eq!(
            Scanner::new("123.345.890").next(),
            Err(ScanError::BadNumber(1, "123.345.890".to_owned()))
        );
    }

    // Strings //

    #[test]
    fn scan_simple_strings() {
        assert_eq!(
            Scanner::new("\"Hello, World!\"").next(),
            Ok(Token::String(Str::from("Hello, World!")))
        );
        assert_eq!(
            Scanner::new("\"\\0 \\t \\\\ \\n \\\" \"").next(),
            Ok(Token::String(Str::from("\0 \t \\ \n \" ")))
        );
    }

    #[test]
    fn scan_invalid_strings() {
        assert_eq!(
            Scanner::new("\"Hello, \n World!\"").next(),
            Err(ScanError::MultiLineString(1))
        );
        assert_eq!(
            Scanner::new("\"Hello, World!").next(),
            Err(ScanError::Eof(1)),
        );
        // Note because the unicode char spans more than one byte the passed
        // \u{2000} is not what we expect as the bad byte/char
        assert_eq!(
            Scanner::new("\"\u{2000}\x02\"").next(),
            Ok(Token::String(Str::from(&vec![24u8, 24u8, 24u8, 24u8][..])))
        );
        // Not exhaustive
        assert_eq!(
            Scanner::new("\"Hello, \\h World!\"").next(),
            Err(ScanError::BadEscape(1, "\\h".to_string()))
        );
    }
}
