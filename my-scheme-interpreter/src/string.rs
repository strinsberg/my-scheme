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

// TODO convert this to Str like Num. Implement the proper rust traits for things
// like ToString and FromStr. There may still be a little work to ensure that
// we get the external vs display representation down how we want it. The use
// of FromStr can replace the constructor, or also be used in the constructor
// when using a string. This can parse the inside of a string, not including the
// quotes. It will iterate with .chars() and do something when encountering non-ascii
// characters rather than accepting raw bytes. Which may require some adjustments
// on the scanner or reader.

// Scheme strings //////////////////////////////////////////////////////////////

// TODO rewrite like Num, Cell, and Env with rust traits implemented for creating from
// strings and bytes. Move the char to its own file unless it is really small
// afterward, because both need to be tested.
// TODO add the string builtins into this class and just do type and arity checks
// in the core procs. Some things might still be good in scheme, but it is so
// much better for being informative about errors etc. if we do as much in
// rust as possible and putting it in classes organizes it nicely. I.e. the rust
// types are the types we have in the scheme interpreter and their methods are the
// functions called on them except with the wrapper of the Val type.

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ScmString {
    // add a mutable flag
    // make this private so that it cannot be adjusted, and put it into a ref cell
    pub chars: Vec<ScmChar>,
}

impl ScmString {
    // Add constructors that take value and size
    // Add methods like get and set and borrow_chars like in the vector class
    // Add methods for length and mutability
    // Basically copy the vector class

    // make this a from trait or do like in so it can be mutable and immutable
    pub fn new(string: &str) -> ScmString {
        ScmString {
            chars: string.chars().map(|ch| ScmChar::from_char(ch)).collect(),
        }
    }

    // make this a from trait and use &[u8] instead, can use from because
    // if we are creating from bytes it is an immutable string for sure.
    pub fn from_bytes(bytes: &Vec<u8>) -> ScmString {
        ScmString {
            chars: bytes.iter().map(|b| ScmChar::new(*b)).collect(),
        }
    }

    // Do this in display trait
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

    // Do this in an ExternalRep trait
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

// Make this nicer??
impl fmt::Debug for ScmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .chars
            .iter()
            .map(|ch| format!("{}", ch.to_extern()))
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

// Adjust like above in some ways, though this is simpler. Maybe refer to Num.
impl ScmChar {
    // Implement this with a from trait
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

    // Implement this with a from trait
    pub fn from_char(ch: char) -> ScmChar {
        ScmChar::new(ch as u8)
    }

    // Implement this in Display trait
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

    // Implement this in an ExternalRep trait
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

    // Organize these better as predicates and conversions
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
            ScmChar::Null => false,
            ScmChar::Tab => true,
            ScmChar::LineFeed => true,
            ScmChar::Space => true,
            ScmChar::Char(byte) => (*byte as char).is_ascii_whitespace(),
            ScmChar::Unsupported => false,
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

    pub fn to_upper_case(&self) -> ScmChar {
        ScmChar::from_char((self.to_byte() as char).to_ascii_uppercase())
    }

    pub fn to_lower_case(&self) -> ScmChar {
        ScmChar::from_char((self.to_byte() as char).to_ascii_lowercase())
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
