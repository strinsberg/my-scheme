use crate::char::Char;
use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Str {
    chars: Vec<Char>,
}

impl Str {
    pub fn get(&self, idx: usize) -> Result<&Char, Error> {
        if idx < self.chars.len() {
            Ok(&self.chars[idx])
        } else {
            Err(Error::OutOfRange)
        }
    }

    pub fn len(&self) -> usize {
        self.chars.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Char> {
        self.chars.iter()
    }

    // Functions //

    pub fn append(&self, other: Str) -> Str {
        let mut chars = self.chars.clone();
        for ch in other.iter() {
            chars.push(ch.clone())
        }
        Str::from(chars)
    }

    pub fn substring(&self, start: usize, end: usize) -> Str {
        let mut chars = vec![];
        let last = match end {
            i if i >= self.chars.len() => self.chars.len(),
            i => i,
        };
        for i in start..last {
            chars.push(self.chars[i].clone())
        }
        Str::from(chars)
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl Default for Str {
    fn default() -> Str {
        Str::from("")
    }
}

impl From<&str> for Str {
    fn from(s: &str) -> Str {
        Str {
            chars: s.chars().map(|ch| Char::from(ch)).collect(),
        }
    }
}

impl From<&[u8]> for Str {
    fn from(bytes: &[u8]) -> Str {
        Str {
            chars: bytes.iter().map(|b| Char::from(*b)).collect(),
        }
    }
}

impl From<Vec<Char>> for Str {
    fn from(chars: Vec<Char>) -> Str {
        Str { chars: chars }
    }
}

impl DisplayRep for Str {
    fn to_display(&self) -> String {
        let mut strings = vec![];
        for ch in self.chars.iter() {
            strings.push(match ch {
                Char::Null => "\0".to_owned(),
                Char::Tab => "\t".to_owned(),
                Char::LineFeed => "\n".to_owned(),
                Char::Space => " ".to_owned(),
                Char::Unsupported => "UNSUP".to_owned(),
                _ => ch.to_display(),
            })
        }
        strings.join("")
    }
}

impl ExternalRep for Str {
    fn to_external(&self) -> String {
        let mut strings = vec!["\"".to_owned()];
        for ch in self.chars.iter() {
            strings.push(match ch {
                Char::Null => "\0".to_owned(),
                Char::Tab => "\t".to_owned(),
                Char::LineFeed => "\n".to_owned(),
                Char::Space => " ".to_owned(),
                Char::Unsupported => "UNSUP".to_owned(),
                _ => ch.to_external(),
            })
        }
        strings.push("\"".to_owned());
        strings.join("")
    }
}

// Testing ////////////////////////////////////////////////////////////////////
