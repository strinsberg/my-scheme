use std::fmt;
use std::hash::{Hash, Hasher};

// Number type
// Currently only has i64 and f64.

// TODO testing

#[derive(Debug, Clone, PartialEq)]
pub enum ScmNumber {
    Integer(i64),
    Float(f64),
    //Rational(i64, i64),
}

impl ScmNumber {
    pub fn from_str(s: &str) -> Option<ScmNumber> {
        match s.parse::<i64>() {
            Ok(i) => Some(ScmNumber::Integer(i)),
            Err(_) => match s.parse::<f64>() {
                Ok(f) => Some(ScmNumber::Float(f)),
                Err(_) => None,
            },
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            ScmNumber::Integer(i) => i.to_string(),
            ScmNumber::Float(f) => f.to_string(),
        }
    }

    // Basic Arithmetic //
    //
    // These are simple this way, but when there are more types it might require
    // more than a simple condition to decide what number type will be returned.

    pub fn add(&self, right: ScmNumber) -> Option<ScmNumber> {
        if self.is_float() || right.is_float() {
            Some(ScmNumber::Float(self.as_f64() + right.as_f64()))
        } else {
            Some(ScmNumber::Integer(self.as_i64() + right.as_i64()))
        }
    }

    pub fn subtract(&self, right: ScmNumber) -> Option<ScmNumber> {
        if self.is_float() || right.is_float() {
            Some(ScmNumber::Float(self.as_f64() - right.as_f64()))
        } else {
            Some(ScmNumber::Integer(self.as_i64() - right.as_i64()))
        }
    }

    pub fn multiply(&self, right: ScmNumber) -> Option<ScmNumber> {
        if self.is_float() || right.is_float() {
            Some(ScmNumber::Float(self.as_f64() * right.as_f64()))
        } else {
            Some(ScmNumber::Integer(self.as_i64() * right.as_i64()))
        }
    }

    pub fn divide(&self, right: ScmNumber) -> Option<ScmNumber> {
        if right.is_zero() {
            return None;
        } else {
            Some(ScmNumber::Float(self.as_f64() / right.as_f64()))
        }
    }

    pub fn negate(&self) -> ScmNumber {
        match self {
            ScmNumber::Integer(i) => ScmNumber::Integer(0 - i),
            ScmNumber::Float(f) => ScmNumber::Float(0.0 - f),
        }
    }

    pub fn invert(&self) -> Option<ScmNumber> {
        ScmNumber::Float(1.0).divide(self.clone())
    }

    // Helpers //

    pub fn is_zero(&self) -> bool {
        match *self {
            ScmNumber::Integer(i) => i == 0,
            ScmNumber::Float(f) => f == 0.0,
        }
    }

    fn as_i64(&self) -> i64 {
        match *self {
            ScmNumber::Integer(i) => i,
            ScmNumber::Float(f) => f as i64,
        }
    }

    fn as_f64(&self) -> f64 {
        match *self {
            ScmNumber::Integer(i) => i as f64,
            ScmNumber::Float(f) => f,
        }
    }

    fn _is_int(&self) -> bool {
        match *self {
            ScmNumber::Integer(_) => true,
            ScmNumber::Float(_) => false,
        }
    }

    fn is_float(&self) -> bool {
        match *self {
            ScmNumber::Integer(_) => false,
            ScmNumber::Float(_) => true,
        }
    }
}

// TODO the float hash is a hack and could cause problems, but equality on
// floats is just not a guarantee. I suppose it would be smart to discourage
// using floats as hash keys. For now at least only the env actually uses
// the hash map and will always use symbols.
impl Hash for ScmNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            ScmNumber::Integer(i) => i.hash(state),
            ScmNumber::Float(f) => format!("{f}").hash(state),
        }
    }
}

impl Eq for ScmNumber {}

impl fmt::Display for ScmNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScmNumber::Integer(n) => write!(f, "{n}"),
            ScmNumber::Float(n) => write!(f, "{n}"),
        }
    }
}
