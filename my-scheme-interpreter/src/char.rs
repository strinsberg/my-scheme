use crate::rep::{DisplayRep, ExternalRep};
use std::fmt;

// Scheme characters ///////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Char {
    Char(u8),
    Null,
    Tab,
    LineFeed,
    Space,
    Unsupported,
}

impl Char {
    // Conversion //

    pub fn to_byte(&self) -> u8 {
        match self {
            Char::Null => 0,
            Char::Tab => 9,
            Char::LineFeed => 10,
            Char::Space => 32,
            Char::Char(byte) => *byte,
            Char::Unsupported => 24,
        }
    }

    pub fn to_int(&self) -> i64 {
        self.to_byte() as i64
    }

    pub fn to_upper_case(&self) -> Char {
        Char::from((self.to_byte() as char).to_ascii_uppercase())
    }

    pub fn to_lower_case(&self) -> Char {
        Char::from((self.to_byte() as char).to_ascii_lowercase())
    }

    // Predicates //

    pub fn is_alpha(&self) -> bool {
        (self.to_byte() as char).is_ascii_alphabetic()
    }

    pub fn is_alphanumeric(&self) -> bool {
        (self.to_byte() as char).is_ascii_alphanumeric()
    }

    pub fn is_numeric(&self) -> bool {
        (self.to_byte() as char).is_ascii_digit()
    }

    pub fn is_whitespace(&self) -> bool {
        match self {
            Char::Null => false,
            Char::Tab => true,
            Char::LineFeed => true,
            Char::Space => true,
            Char::Char(byte) => (*byte as char).is_ascii_whitespace(),
            Char::Unsupported => false,
        }
    }

    pub fn is_unsup(&self) -> bool {
        match self {
            Char::Unsupported => true,
            _ => false,
        }
    }

    pub fn is_upper_case(&self) -> bool {
        self.to_byte() >= 'A' as u8 && self.to_byte() <= 'Z' as u8
    }

    pub fn is_lower_case(&self) -> bool {
        self.to_byte() >= 'a' as u8 && self.to_byte() <= 'z' as u8
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl Default for Char {
    fn default() -> Char {
        Char::Char('\0' as u8)
    }
}

impl From<u8> for Char {
    fn from(byte: u8) -> Char {
        match byte {
            0 => Char::Null,
            9 => Char::Tab,
            10 => Char::LineFeed,
            32 => Char::Space,
            33..=126 => Char::Char(byte),
            _ => Char::Unsupported,
        }
    }
}

impl From<char> for Char {
    fn from(ch: char) -> Char {
        Char::from(ch as u8)
    }
}

impl DisplayRep for Char {
    fn to_display(&self) -> String {
        match self {
            Char::Null => "\0".to_string(),
            Char::Tab => "\t".to_string(),
            Char::LineFeed => "\n".to_string(),
            Char::Space => " ".to_string(),
            Char::Char(byte) => format!("{}", *byte as char),
            Char::Unsupported => "\x18".to_string(),
        }
    }
}

impl ExternalRep for Char {
    fn to_external(&self) -> String {
        match self {
            Char::Null => "#\\null".to_string(),
            Char::Tab => "#\\tab".to_string(),
            Char::LineFeed => "#\\newline".to_string(),
            Char::Space => "#\\space".to_string(),
            Char::Char(byte) => format!("#\\{}", *byte as char),
            Char::Unsupported => "#\\unsup".to_string(),
        }
    }
}

impl fmt::Debug for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Char({})", self.to_external())
    }
}

// Testing ////////////////////////////////////////////////////////////////////
