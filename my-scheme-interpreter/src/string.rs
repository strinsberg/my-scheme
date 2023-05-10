use std::fmt;

// An R5RS compliant string and characters.
// Only allows ascii chars and a few escape sequences. All other bytes
// will be turned into #\UNSUP without any errors.
//
// NOTE unsupported characters that use more than 1 byte may become multiple
// ScmChars and cause unsuspected results as some of those bytes may be inside
// ascii range even though the larger char is not ascii.
// For example, if a char had the byte sequence [250, 5, 33] would be
// [#\UNSUP, #\UNSUP, #\!] (not sure this example byte sequence is possible).

// Scheme strings //////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ScmString {
    pub chars: Vec<ScmChar>,
}

impl ScmString {
    pub fn new(string: &str) -> ScmString {
        ScmString {
            chars: string.chars().map(|ch| ScmChar::from_char(ch)).collect(),
        }
    }

    pub fn from_bytes(bytes: &Vec<u8>) -> ScmString {
        ScmString {
            chars: bytes.iter().map(|b| ScmChar::new(*b)).collect(),
        }
    }

    pub fn to_string(&self) -> String {
        self.chars
            .iter()
            .map(|ascii| match ascii {
                ScmChar::Null => "\0".to_owned(),
                ScmChar::Tab => "\t".to_owned(),
                ScmChar::LineFeed => "\n".to_owned(),
                ScmChar::Space => " ".to_owned(),
                ScmChar::Unsupported => panic!(
                    "ScmString should not contain unsupported ScmChars: {:?}",
                    ascii
                ),
                _ => ascii.to_string(),
            })
            .collect()
    }

    pub fn to_extern(&self) -> String {
        let string: String = self
            .chars
            .iter()
            .map(|ascii| match ascii {
                ScmChar::Null => "\\0".to_owned(),
                ScmChar::Tab => "\\t".to_owned(),
                ScmChar::LineFeed => "\\n".to_owned(),
                ScmChar::Space => " ".to_owned(),
                ScmChar::Unsupported => panic!(
                    "ScmString should not contain unsupported ScmChars: {:?}",
                    ascii
                ),
                ScmChar::Char(ch) if *ch == '"' as u8 => "\\\"".to_owned(),
                _ => ascii.to_string(),
            })
            .collect();
        format!("\"{}\"", string)
    }
}

impl fmt::Display for ScmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Debug for ScmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .chars
            .iter()
            .map(|ch| format!("{:?}", ch))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "ScmString[{}]", s)
    }
}

// Scheme characters ///////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ScmChar {
    Char(u8),
    Null,
    Tab,
    LineFeed,
    Space,
    Unsupported,
}

impl ScmChar {
    pub fn new(byte: u8) -> ScmChar {
        match byte {
            0 => ScmChar::Null,
            9 => ScmChar::Tab,
            10 => ScmChar::LineFeed,
            32 => ScmChar::Space,
            33..=126 => ScmChar::Char(byte),
            _ => ScmChar::Unsupported,
        }
    }

    pub fn from_char(ch: char) -> ScmChar {
        ScmChar::new(ch as u8)
    }

    pub fn to_string(&self) -> String {
        match self {
            ScmChar::Null => "\0".to_string(),
            ScmChar::Tab => "\t".to_string(),
            ScmChar::LineFeed => "\n".to_string(),
            ScmChar::Space => " ".to_string(),
            ScmChar::Char(byte) => format!("{}", *byte as char),
            ScmChar::Unsupported => "\x18".to_string(),
        }
    }

    pub fn to_extern(&self) -> String {
        match self {
            ScmChar::Null => "#\\null".to_string(),
            ScmChar::Tab => "#\\tab".to_string(),
            ScmChar::LineFeed => "#\\newline".to_string(),
            ScmChar::Space => "#\\space".to_string(),
            ScmChar::Char(byte) => format!("#\\{}", *byte as char),
            ScmChar::Unsupported => "#\\unsup".to_string(),
        }
    }

    pub fn to_byte(&self) -> u8 {
        match self {
            ScmChar::Null => 0,
            ScmChar::Tab => 9,
            ScmChar::LineFeed => 10,
            ScmChar::Space => 32,
            ScmChar::Char(byte) => *byte,
            ScmChar::Unsupported => 24,
        }
    }

    pub fn to_int(&self) -> i64 {
        self.to_byte() as i64
    }

    pub fn is_alpha(&self) -> bool {
        (self.to_byte() as char).is_alphabetic()
    }

    pub fn is_alphanumeric(&self) -> bool {
        (self.to_byte() as char).is_alphanumeric()
    }

    pub fn is_numeric(&self) -> bool {
        (self.to_byte() as char).is_numeric()
    }

    pub fn is_whitespace(&self) -> bool {
        match self {
            ScmChar::Null => true,
            ScmChar::Tab => true,
            ScmChar::LineFeed => true,
            ScmChar::Space => true,
            ScmChar::Char(byte) => (*byte as char).is_whitespace(),
            ScmChar::Unsupported => true, // should this be whitespce?
        }
    }

    pub fn is_unsup(&self) -> bool {
        match self {
            ScmChar::Unsupported => true,
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

impl fmt::Display for ScmChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Debug for ScmChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScmChar({})", self)
    }
}
