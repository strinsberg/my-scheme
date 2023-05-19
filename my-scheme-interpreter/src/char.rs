use crate::rep::{DisplayRep, ExternalRep};
use std::fmt;

// Scheme characters ///////////////////////////////////////////////////////////

const NULL: u8 = '\0' as u8;
const LF: u8 = '\n' as u8;
const TAB: u8 = '\t' as u8;
const SPACE: u8 = ' ' as u8;
const UNSUP: u8 = 24;
const FIRST_ASCII: u8 = 33;
const LAST_ASCII: u8 = 126;

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
            Char::Null => NULL,
            Char::Tab => TAB,
            Char::LineFeed => LF,
            Char::Space => SPACE,
            Char::Char(byte) => *byte,
            Char::Unsupported => UNSUP,
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
            NULL => Char::Null,
            TAB => Char::Tab,
            LF => Char::LineFeed,
            SPACE => Char::Space,
            FIRST_ASCII..=LAST_ASCII => Char::Char(byte),
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
            Char::Unsupported => "**UNSUP**".to_string(),
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

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl fmt::Debug for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Char({})", self.to_external())
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_char_from_u8_and_display() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let ch = Char::from(i);
            let other = i as char;
            assert_eq!(ch.to_display(), String::from(other));
        }

        assert_eq!(Char::from(NULL).to_display(), "\0".to_owned());
        assert_eq!(Char::from(TAB).to_display(), "\t".to_owned());
        assert_eq!(Char::from(LF).to_display(), "\n".to_owned());
        assert_eq!(Char::from(SPACE).to_display(), " ".to_owned());
        assert_eq!(Char::from(UNSUP).to_display(), "**UNSUP**".to_owned());
    }

    fn test_char_from_char_and_display() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let other = i as char;
            let ch = Char::from(other);
            assert_eq!(ch.to_display(), String::from(other));
        }

        assert_eq!(Char::from('\0').to_display(), "\0".to_owned());
        assert_eq!(Char::from('\t').to_display(), "\t".to_owned());
        assert_eq!(Char::from('\n').to_display(), "\n".to_owned());
        assert_eq!(Char::from(' ').to_display(), " ".to_owned());
        assert_eq!(Char::from('\x18').to_display(), "**UNSUP**".to_owned());
    }

    #[test]
    fn test_char_from_u8_and_external() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let ch = Char::from(i);
            let other = i as char;
            assert_eq!(ch.to_external(), format!("#\\{other}"));
        }

        assert_eq!(Char::from(NULL).to_external(), "#\\null".to_owned());
        assert_eq!(Char::from(TAB).to_external(), "#\\tab".to_owned());
        assert_eq!(Char::from(LF).to_external(), "#\\newline".to_owned());
        assert_eq!(Char::from(SPACE).to_external(), "#\\space".to_owned());
        assert_eq!(Char::from(UNSUP).to_external(), "#\\unsup".to_owned());
    }

    #[test]
    fn test_char_is_whitespace() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let ch = Char::from(i);
            assert!(!ch.is_whitespace());
        }

        assert!(Char::from(TAB).is_whitespace());
        assert!(Char::from(LF).is_whitespace());
        assert!(Char::from(SPACE).is_whitespace());

        assert!(!Char::from(NULL).is_whitespace());
        assert!(!Char::from(UNSUP).is_whitespace());
    }

    #[test]
    fn test_char_is_alpha_and_numeric() {
        for i in 'a' as u8..='z' as u8 {
            let ch = Char::from(i);
            assert!(ch.is_alpha());
            assert!(ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        for i in 'A' as u8..='Z' as u8 {
            let ch = Char::from(i);
            assert!(ch.is_alpha());
            assert!(ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        for i in '0' as u8..='9' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_alpha());
            assert!(ch.is_alphanumeric());
            assert!(ch.is_numeric());
        }

        for i in FIRST_ASCII..'0' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_alpha());
            assert!(!ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        for i in ':' as u8..'A' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_alpha());
            assert!(!ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        for i in '[' as u8..'a' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_alpha());
            assert!(!ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        for i in '{' as u8..=LAST_ASCII {
            let ch = Char::from(i);
            assert!(!ch.is_alpha());
            assert!(!ch.is_alphanumeric());
            assert!(!ch.is_numeric());
        }

        assert!(!Char::from(NULL).is_alpha());
        assert!(!Char::from(TAB).is_alpha());
        assert!(!Char::from(LF).is_alpha());
        assert!(!Char::from(SPACE).is_alpha());
        assert!(!Char::from(UNSUP).is_alpha());

        assert!(!Char::from(NULL).is_alphanumeric());
        assert!(!Char::from(TAB).is_alphanumeric());
        assert!(!Char::from(LF).is_alphanumeric());
        assert!(!Char::from(SPACE).is_alphanumeric());
        assert!(!Char::from(UNSUP).is_alphanumeric());

        assert!(!Char::from(NULL).is_numeric());
        assert!(!Char::from(TAB).is_numeric());
        assert!(!Char::from(LF).is_numeric());
        assert!(!Char::from(SPACE).is_numeric());
        assert!(!Char::from(UNSUP).is_numeric());
    }

    #[test]
    fn test_char_is_unsup() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let ch = Char::from(i);
            assert!(!ch.is_unsup());
        }

        assert!(!Char::from(TAB).is_unsup());
        assert!(!Char::from(LF).is_unsup());
        assert!(!Char::from(SPACE).is_unsup());
        assert!(!Char::from(NULL).is_unsup());

        assert!(Char::from(UNSUP).is_unsup());

        for i in NULL + 1..TAB {
            let ch = Char::from(i);
            assert!(ch.is_unsup());
        }

        for i in TAB + 1..LF {
            let ch = Char::from(i);
            assert!(ch.is_unsup());
        }

        for i in LF + 1..SPACE {
            let ch = Char::from(i);
            assert!(ch.is_unsup());
        }

        assert!(!Char::from(LAST_ASCII).is_unsup());
        assert!(Char::from(127).is_unsup());
    }

    #[test]
    fn test_char_is_upper_and_to_upper() {
        for i in 'A' as u8..='Z' as u8 {
            let ch = Char::from(i);
            assert!(ch.is_upper_case());
            assert!(ch.to_upper_case().is_upper_case());
        }

        for i in FIRST_ASCII..'A' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_upper_case());
            assert!(!ch.to_upper_case().is_upper_case());
        }

        for i in 'Z' as u8 + 1..'a' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_upper_case());
            assert!(!ch.to_upper_case().is_upper_case());
        }

        for i in 'a' as u8..='z' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_upper_case());
            assert!(ch.to_upper_case().is_upper_case());
        }

        for i in 'z' as u8 + 1..=LAST_ASCII {
            let ch = Char::from(i);
            assert!(!ch.is_upper_case());
            assert!(!ch.to_upper_case().is_upper_case());
        }

        assert!(!Char::from(TAB).is_upper_case());
        assert!(!Char::from(LF).is_upper_case());
        assert!(!Char::from(SPACE).is_upper_case());
        assert!(!Char::from(NULL).is_upper_case());
        assert!(!Char::from(UNSUP).is_upper_case());

        assert!(!Char::from(TAB).to_upper_case().is_upper_case());
        assert!(!Char::from(LF).to_upper_case().is_upper_case());
        assert!(!Char::from(SPACE).to_upper_case().is_upper_case());
        assert!(!Char::from(NULL).to_upper_case().is_upper_case());
        assert!(!Char::from(UNSUP).to_upper_case().is_upper_case());
    }

    #[test]
    fn test_char_is_lower_and_to_lower() {
        for i in 'a' as u8..='z' as u8 {
            let ch = Char::from(i);
            assert!(ch.is_lower_case());
            assert!(ch.to_lower_case().is_lower_case());
        }

        for i in 'A' as u8..='Z' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_lower_case());
            assert!(ch.to_lower_case().is_lower_case());
        }

        for i in FIRST_ASCII..'A' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_lower_case());
            assert!(!ch.to_lower_case().is_lower_case());
        }

        for i in 'Z' as u8 + 1..'a' as u8 {
            let ch = Char::from(i);
            assert!(!ch.is_lower_case());
            assert!(!ch.to_lower_case().is_lower_case());
        }

        for i in 'z' as u8 + 1..=LAST_ASCII {
            let ch = Char::from(i);
            assert!(!ch.is_lower_case());
            assert!(!ch.to_lower_case().is_lower_case());
        }

        assert!(!Char::from(TAB).is_lower_case());
        assert!(!Char::from(LF).is_lower_case());
        assert!(!Char::from(SPACE).is_lower_case());
        assert!(!Char::from(NULL).is_lower_case());
        assert!(!Char::from(UNSUP).is_lower_case());

        assert!(!Char::from(TAB).to_lower_case().is_lower_case());
        assert!(!Char::from(LF).to_lower_case().is_lower_case());
        assert!(!Char::from(SPACE).to_lower_case().is_lower_case());
        assert!(!Char::from(NULL).to_lower_case().is_lower_case());
        assert!(!Char::from(UNSUP).to_lower_case().is_lower_case());
    }

    #[test]
    fn test_char_additional_to_upper() {
        assert_eq!(Char::from('a').to_upper_case(), Char::from('A'));
        assert_eq!(Char::from('h').to_upper_case(), Char::from('H'));
        assert_eq!(Char::from('s').to_upper_case(), Char::from('S'));
        assert_eq!(Char::from('z').to_upper_case(), Char::from('Z'));

        assert_eq!(Char::from('Z').to_upper_case(), Char::from('Z'));
        assert_eq!(Char::from('.').to_upper_case(), Char::from('.'));
        assert_eq!(Char::from(':').to_upper_case(), Char::from(':'));
        assert_eq!(Char::from('-').to_upper_case(), Char::from('-'));
        assert_eq!(Char::from('8').to_upper_case(), Char::from('8'));

        assert_eq!(Char::from(TAB).to_upper_case(), Char::from(TAB));
        assert_eq!(Char::from(LF).to_upper_case(), Char::from(LF));
        assert_eq!(Char::from(SPACE).to_upper_case(), Char::from(SPACE));
        assert_eq!(Char::from(NULL).to_upper_case(), Char::from(NULL));
        assert_eq!(Char::from(UNSUP).to_upper_case(), Char::from(UNSUP));
    }

    #[test]
    fn test_char_additional_to_lower() {
        assert_eq!(Char::from('A').to_lower_case(), Char::from('a'));
        assert_eq!(Char::from('H').to_lower_case(), Char::from('h'));
        assert_eq!(Char::from('S').to_lower_case(), Char::from('s'));
        assert_eq!(Char::from('Z').to_lower_case(), Char::from('z'));

        assert_eq!(Char::from('a').to_lower_case(), Char::from('a'));
        assert_eq!(Char::from('.').to_lower_case(), Char::from('.'));
        assert_eq!(Char::from(':').to_lower_case(), Char::from(':'));
        assert_eq!(Char::from('-').to_lower_case(), Char::from('-'));
        assert_eq!(Char::from('8').to_lower_case(), Char::from('8'));

        assert_eq!(Char::from(TAB).to_lower_case(), Char::from(TAB));
        assert_eq!(Char::from(LF).to_lower_case(), Char::from(LF));
        assert_eq!(Char::from(SPACE).to_lower_case(), Char::from(SPACE));
        assert_eq!(Char::from(NULL).to_lower_case(), Char::from(NULL));
        assert_eq!(Char::from(UNSUP).to_lower_case(), Char::from(UNSUP));
    }

    #[test]
    fn test_char_to_byte_and_to_int() {
        for i in FIRST_ASCII..=LAST_ASCII {
            let ch = Char::from(i);
            assert_eq!(ch.to_byte(), i);
            assert_eq!(ch.to_int(), i as i64);
        }

        assert_eq!(Char::from('\t').to_byte(), TAB);
        assert_eq!(Char::from('\n').to_byte(), LF);
        assert_eq!(Char::from(' ').to_byte(), SPACE);
        assert_eq!(Char::from('\0').to_byte(), NULL);
        assert_eq!(Char::from('\x18').to_byte(), UNSUP);

        assert_eq!(Char::from('\t').to_int(), TAB as i64);
        assert_eq!(Char::from('\n').to_int(), LF as i64);
        assert_eq!(Char::from(' ').to_int(), SPACE as i64);
        assert_eq!(Char::from('\0').to_int(), NULL as i64);
        assert_eq!(Char::from('\x18').to_int(), UNSUP as i64);
    }
}
