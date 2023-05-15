use std::cmp::Ordering;
use std::fmt;

// Number type
// Currently only has i64 and f64.

// TODO add necessary arithmetic functions from the report that are needed
// to implement others in core_proc or in scheme code.
// TODO not in here, but do some work in the scanner and reader to accept the
// different input literals that are supposed to be supported. We will also
// need to be able to print/display them in the same way rather than just relying
// on rust to do this for us.
// TODO add the Rational type for exact non-integer numbers
// TODO the order will always put int before float this way... Implement it
// yourself to ensure that ints can be converted and compared properly.
// TODO testing
//
// TODO try implementing some things like From, Display, Default,
// FromStr (which means that in scanner we could look for starting tokens and pass
// to parse like we already do, but it would give these number types)

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ScmNumber {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
}

impl ScmNumber {
    pub fn make_rational(a: i64, b: i64) -> Option<ScmNumber> {
        // denominator cannot be 0 unless the whole thing is 0/0,
        // and if the whole thing is 0 just return an integer,
        // same if the denominator is 1,
        // if denom is negative swap the sign with numer,
        // if both are negative make them both positive.
        if b == 0 {
            None
        } else if a == 0 {
            Some(ScmNumber::Integer(0))
        } else if b == 1 {
            Some(ScmNumber::Integer(a))
        } else if b < 0 {
            Some(ScmNumber::Rational(-a, -b))
        } else {
            Some(ScmNumber::Rational(a, b))
        }
    }

    // TODO no rationals from strings this way, this probably needs to be replaced
    // by the scanner and reader and just not exist here.
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
            ScmNumber::Rational(n, d) => format!("{}/{}", n, d),
        }
    }

    // Basic Arithmetic //
    //
    // These are simple this way, but when there are more types it might require
    // more than a simple condition to decide what number type will be returned.

    pub fn add(&self, right: &ScmNumber) -> Option<ScmNumber> {
        match (self, right) {
            (ScmNumber::Float(_), _) | (_, ScmNumber::Float(_)) => {
                Some(ScmNumber::Float(self.as_f64() + right.as_f64()))
            }
            (ScmNumber::Rational(a, b), ScmNumber::Rational(c, d)) => {
                ScmNumber::simplify_fraction((a * d) + (b * c), b * d)
            }
            (ScmNumber::Rational(a, b), ScmNumber::Integer(i))
            | (ScmNumber::Integer(i), ScmNumber::Rational(a, b)) => {
                ScmNumber::simplify_fraction(a + (b * i), *b)
            }
            (_, _) => Some(ScmNumber::Integer(self.as_i64() + right.as_i64())),
        }
    }

    pub fn subtract(&self, right: &ScmNumber) -> Option<ScmNumber> {
        match (self, right) {
            (ScmNumber::Float(_), _) | (_, ScmNumber::Float(_)) => {
                Some(ScmNumber::Float(self.as_f64() - right.as_f64()))
            }
            (ScmNumber::Rational(a, b), ScmNumber::Rational(c, d)) => {
                ScmNumber::simplify_fraction((a * d) - (b * c), b * d)
            }
            (ScmNumber::Rational(a, b), ScmNumber::Integer(i))
            | (ScmNumber::Integer(i), ScmNumber::Rational(a, b)) => {
                ScmNumber::simplify_fraction(a - (b * i), *b)
            }
            (_, _) => Some(ScmNumber::Integer(self.as_i64() - right.as_i64())),
        }
    }

    pub fn multiply(&self, right: &ScmNumber) -> Option<ScmNumber> {
        match (self, right) {
            (ScmNumber::Float(_), _) | (_, ScmNumber::Float(_)) => {
                Some(ScmNumber::Float(self.as_f64() * right.as_f64()))
            }
            (ScmNumber::Rational(a, b), ScmNumber::Rational(c, d)) => {
                ScmNumber::simplify_fraction(a * c, b * d)
            }
            (ScmNumber::Rational(a, b), ScmNumber::Integer(i))
            | (ScmNumber::Integer(i), ScmNumber::Rational(a, b)) => {
                ScmNumber::simplify_fraction(a * i, *b)
            }
            (_, _) => Some(ScmNumber::Integer(self.as_i64() * right.as_i64())),
        }
    }

    pub fn divide(&self, right: &ScmNumber) -> Option<ScmNumber> {
        if right.is_zero() {
            return None;
        }

        match (self, right) {
            (ScmNumber::Integer(a), ScmNumber::Integer(b)) => ScmNumber::simplify_fraction(*a, *b),
            (ScmNumber::Rational(a, b), ScmNumber::Rational(c, d)) => {
                ScmNumber::simplify_fraction(a * d, b * c)
            }
            (ScmNumber::Rational(a, b), ScmNumber::Integer(i))
            | (ScmNumber::Integer(i), ScmNumber::Rational(a, b)) => {
                ScmNumber::simplify_fraction(*a, b * i)
            }
            (_, _) => Some(ScmNumber::Float(self.as_f64() / right.as_f64())),
        }
    }

    pub fn negate(&self) -> ScmNumber {
        match self {
            ScmNumber::Integer(i) => ScmNumber::Integer(-i),
            ScmNumber::Rational(a, b) => ScmNumber::Rational(-a, -b),
            ScmNumber::Float(f) => ScmNumber::Float(-f),
        }
    }

    pub fn invert(&self) -> Option<ScmNumber> {
        match self {
            ScmNumber::Integer(i) => Some(ScmNumber::Rational(1, *i)),
            ScmNumber::Rational(a, b) => Some(ScmNumber::Rational(*b, *a)),
            ScmNumber::Float(f) => Some(ScmNumber::Float(1.0 / f)),
        }
    }

    // Helpers //

    pub fn simplify_fraction(n: i64, d: i64) -> Option<ScmNumber> {
        let m = gcd(abs(n) as u64, abs(d) as u64) as i64;
        ScmNumber::make_rational(n / m, d / m)
    }

    pub fn is_zero(&self) -> bool {
        match *self {
            ScmNumber::Integer(i) => i == 0,
            ScmNumber::Rational(a, b) => a == 0 && b == 0,
            ScmNumber::Float(f) => f == 0.0,
        }
    }

    fn as_i64(&self) -> i64 {
        match *self {
            ScmNumber::Integer(i) => i,
            ScmNumber::Rational(a, b) => a / b, // truncates
            ScmNumber::Float(f) => f as i64,
        }
    }

    fn as_f64(&self) -> f64 {
        match *self {
            ScmNumber::Integer(i) => i as f64,
            ScmNumber::Rational(a, b) => a as f64 / b as f64,
            ScmNumber::Float(f) => f,
        }
    }

    fn is_int(&self) -> bool {
        match *self {
            ScmNumber::Integer(_) => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match *self {
            ScmNumber::Float(_) => true,
            _ => false,
        }
    }

    fn is_rat(&self) -> bool {
        match *self {
            ScmNumber::Rational(_, _) => true,
            _ => false,
        }
    }
}

impl Eq for ScmNumber {}

impl fmt::Display for ScmNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScmNumber::Integer(n) => write!(f, "{n}"),
            ScmNumber::Rational(_, _) => write!(f, "{}", self.to_string()),
            ScmNumber::Float(n) => write!(f, "{n}"),
        }
    }
}

// Other numeric functions ////////////////////////////////////////////////////

pub fn abs(a: i64) -> i64 {
    if a < 0 {
        -a
    } else {
        a
    }
}

// Supposed to be an efficient gcd, copied from wikipedia, uh oh.
// https://en.wikipedia.org/wiki/Binary_GCD_algorithm
pub fn gcd(mut u: u64, mut v: u64) -> u64 {
    use std::cmp::min;
    use std::mem::swap;

    if u == 0 {
        return v;
    } else if v == 0 {
        return u;
    }

    let i = u.trailing_zeros();
    u >>= i;
    let j = v.trailing_zeros();
    v >>= j;
    let k = min(i, j);

    loop {
        debug_assert!(u % 2 == 1, "u = {} is even", u);
        debug_assert!(v % 2 == 1, "v = {} is even", v);

        if u > v {
            swap(&mut u, &mut v);
        }
        v -= u;

        if v == 0 {
            return u << k;
        }

        v >>= v.trailing_zeros();
    }
}

// Num ////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
pub enum Num {
    Int(i64),
    Flt(f64),
    Rat(i64, i64),
}

impl Default for Num {
    fn default() -> Num {
        Num::Int(0)
    }
}

impl Num {
    // construtors //

    pub fn new_rat(a: i64, b: i64) -> Result<Num, ArithErr> {
        if b == 0 {
            Err(ArithErr::DivideByZero)
        } else if a == 0 {
            Ok(Num::default())
        } else if b == 1 {
            Ok(Num::Int(a))
        } else if b < 0 {
            Ok(Num::Rat(-a, -b))
        } else {
            Ok(Num::Rat(a, b))
        }
    }

    // Arithmetic //
    pub fn add(&self, right: &Num) -> Result<Num, ArithErr> {
        match (self, right) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => Ok(Num::Flt(self.as_f64() * right.as_f64())),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify((a * d) + (b * c), b * d),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                Num::simplify(a + (b * i), *b)
            }
            (_, _) => Ok(Num::Int(self.as_i64() * right.as_i64())),
        }
    }

    pub fn sub(&self, right: &Num) -> Result<Num, ArithErr> {
        match (self, right) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => Ok(Num::Flt(self.as_f64() * right.as_f64())),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify((a * d) - (b * c), b * d),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                Num::simplify(a - (b * i), *b)
            }
            (_, _) => Ok(Num::Int(self.as_i64() * right.as_i64())),
        }
    }

    pub fn mult(&self, right: &Num) -> Result<Num, ArithErr> {
        match (self, right) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => Ok(Num::Flt(self.as_f64() * right.as_f64())),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify(a * c, b * d),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                Num::simplify(a * i, *b)
            }
            (_, _) => Ok(Num::Int(self.as_i64() * right.as_i64())),
        }
    }

    pub fn div(&self, right: &Num) -> Result<Num, ArithErr> {
        if right.is_zero() {
            return Err(ArithErr::DivideByZero);
        }

        match (self, right) {
            (Num::Int(a), Num::Int(b)) => Num::simplify(*a, *b),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify(a * d, b * c),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                Num::simplify(*a, b * i)
            }
            (_, _) => Ok(Num::Flt(self.as_f64() / right.as_f64())),
        }
    }
    pub fn negate(&self) -> Num {
        match self {
            Num::Int(i) => Num::Int(-i),
            Num::Flt(f) => Num::Flt(-f),
            Num::Rat(a, b) => Num::new_rat(-a, -b).expect("rational should not have invalid state"),
        }
    }

    pub fn invert(&self) -> Num {
        match self {
            Num::Int(i) => Num::Rat(1, *i),
            Num::Flt(f) => Num::Flt(1.0 / f),
            Num::Rat(a, b) => Num::Rat(*b, *a),
        }
    }

    pub fn simplify(n: i64, d: i64) -> Result<Num, ArithErr> {
        let m = gcd(abs(n) as u64, abs(d) as u64) as i64;
        Num::new_rat(n / m, d / m)
    }

    // Conversion Helpers //

    fn as_i64(&self) -> i64 {
        match *self {
            Num::Int(i) => i,
            Num::Flt(f) => f as i64,
            Num::Rat(a, b) => a / b,
        }
    }

    fn as_f64(&self) -> f64 {
        match *self {
            Num::Int(i) => i as f64,
            Num::Flt(f) => f,
            Num::Rat(a, b) => a as f64 / b as f64,
        }
    }

    // Preidcates //

    pub fn is_zero(&self) -> bool {
        match *self {
            Num::Int(i) => i == 0,
            Num::Flt(f) => f == 0.0,
            Num::Rat(a, _) => a == 0,
        }
    }

    pub fn is_int(&self) -> bool {
        match *self {
            Num::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_flt(&self) -> bool {
        match *self {
            Num::Flt(_) => true,
            _ => false,
        }
    }

    pub fn is_rat(&self) -> bool {
        match *self {
            Num::Rat(_, _) => true,
            _ => false,
        }
    }
}

// We only support binary and octal with their prefix and optional sign, NO exponent
// We support hex with hex prefix, optional sign, and positive exponent only
// We support rational and int with/without decimal prefix and optional sign, NO exponent
// We support float with/without decimal prefix, optional sign, and exponent
// Only exponent marker allowed is e/E
// No exactness is allowed/required, as we have only exact rational, int and inexact float
impl std::str::FromStr for Num {
    type Err = ArithErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("#b") {
            let rust_style = match s.as_bytes()[2] as char {
                '-' => format!("-{}", &s[3..]),
                '+' => format!("{}", &s[3..]),
                _ => format!("{}", &s[2..]),
            };
            return match i64::from_str_radix(&rust_style, 2) {
                Ok(i) => Ok(Num::Int(i)),
                Err(_) => Err(ArithErr::FailedParse(s.to_owned())),
            };
        } else if s.starts_with("#o") {
            let rust_style = match s.as_bytes()[2] as char {
                '-' => format!("-{}", &s[3..]),
                '+' => format!("{}", &s[3..]),
                _ => format!("{}", &s[2..]),
            };
            return match i64::from_str_radix(&rust_style, 8) {
                Ok(i) => Ok(Num::Int(i)),
                Err(_) => Err(ArithErr::FailedParse(s.to_owned())),
            };
        } else if s.starts_with("#x") {
            let rust_style = match s.as_bytes()[2] as char {
                '-' => format!("-{}", &s[3..]),
                '+' => format!("{}", &s[3..]),
                _ => format!("{}", &s[2..]),
            };
            return match i64::from_str_radix(&rust_style, 16) {
                Ok(i) => Ok(Num::Int(i)),
                Err(_) => Err(ArithErr::FailedParse(s.to_owned())),
            };
        }

        let slc = match s.starts_with("#d") {
            true => &s[2..],
            false => &s[..],
        };

        if slc.contains("/") {
            let idx = slc.find("/").unwrap();
            let a = match &slc[..idx].parse::<i64>() {
                Ok(i) => *i,
                Err(_) => return Err(ArithErr::FailedParse(s.to_owned())),
            };
            let b = match &slc[idx + 1..].parse::<i64>() {
                Ok(i) => *i,
                Err(_) => return Err(ArithErr::FailedParse(s.to_owned())),
            };
            Num::new_rat(a, b)
        } else if slc.contains(".") {
            match slc.parse::<f64>() {
                Ok(f) => Ok(Num::Flt(f)),
                Err(_) => return Err(ArithErr::FailedParse(s.to_owned())),
            }
        } else {
            match slc.parse::<i64>() {
                Ok(i) => Ok(Num::Int(i)),
                Err(_) => return Err(ArithErr::FailedParse(s.to_owned())),
            }
        }
    }
}

// TODO should this will work as both display and external, though if it
// is in standard we might want to be able to display ints as other types
// and floats with or without the exponent
impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Num::Int(n) => write!(f, "{n}"),
            Num::Flt(n) => write!(f, "{n}"),
            Num::Rat(a, b) => write!(f, "{}/{}", a, b),
        }
    }
}

impl PartialEq for Num {
    fn eq(&self, other: &Num) -> bool {
        match (self, other) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => self.as_f64().eq(&other.as_f64()),
            (Num::Rat(a, b), Num::Rat(c, d)) => {
                let r1 = Num::simplify(*a, *b).expect("rational should not be in invalid state");
                let r2 = Num::simplify(*c, *d).expect("rational should not be in invalid state");
                match (r1, r2) {
                    (Num::Rat(e, f), Num::Rat(g, h)) => e == g && f == h,
                    _ => false,
                }
            }
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                let r1 = Num::simplify(*a, *b).expect("rational should not be in invalid state");
                let r2 = Num::simplify(*i, 1).expect("rational should not be in invalid state");
                match (r1, r2) {
                    (Num::Rat(e, f), Num::Rat(g, h)) => e == g && f == h,
                    _ => false,
                }
            }
            (Num::Int(x), Num::Int(y)) => x == y,
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
        match (self, other) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => self.as_f64().partial_cmp(&other.as_f64()),
            (Num::Rat(a, b), Num::Rat(c, d)) => compare_int(a * d, b * c),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => compare_int(*a, b * i),
            (Num::Int(x), Num::Int(y)) => compare_int(*x, *y),
        }
    }
}

fn compare_int(a: i64, b: i64) -> Option<Ordering> {
    if a == b {
        Some(Ordering::Equal)
    } else if a < b {
        Some(Ordering::Less)
    } else {
        Some(Ordering::Greater)
    }
}

#[derive(Debug, PartialEq)]
pub enum ArithErr {
    DivideByZero,
    FailedParse(String),
}

// Testing ////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing_decimal_integers() {
        // Simple
        assert_eq!("123".parse::<Num>(), Ok(Num::Int(123)));
        assert_eq!("-123".parse::<Num>(), Ok(Num::Int(-123)));
        assert_eq!("+123".parse::<Num>(), Ok(Num::Int(123)));
        assert_eq!("#d123".parse::<Num>(), Ok(Num::Int(123)));
        assert_eq!("#d-123".parse::<Num>(), Ok(Num::Int(-123)));
        assert_eq!("#d+123".parse::<Num>(), Ok(Num::Int(123)));
        // With exponent is error
        assert_eq!(
            "123e2".parse::<Num>(),
            Err(ArithErr::FailedParse("123e2".to_owned()))
        );
        assert_eq!(
            "123e-2".parse::<Num>(),
            Err(ArithErr::FailedParse("123e-2".to_owned()))
        );
    }

    #[test]
    fn test_parsing_decimal_flaots() {
        // Simple
        assert_eq!("1.23".parse::<Num>(), Ok(Num::Flt(1.23)));
        assert_eq!("-1.23".parse::<Num>(), Ok(Num::Flt(-1.23)));
        assert_eq!("+1.23".parse::<Num>(), Ok(Num::Flt(1.23)));
        assert_eq!("#d1.23".parse::<Num>(), Ok(Num::Flt(1.23)));
        assert_eq!("#d-1.23".parse::<Num>(), Ok(Num::Flt(-1.23)));
        assert_eq!("#d+1.23".parse::<Num>(), Ok(Num::Flt(1.23)));
        // With exponent
        assert_eq!("1.23e2".parse::<Num>(), Ok(Num::Flt(123.0)));
        assert_eq!("1.23e-2".parse::<Num>(), Ok(Num::Flt(0.0123)));
    }

    #[test]
    fn test_parsing_decimal_rationals() {
        // Simple
        assert_eq!("1/2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("-1/2".parse::<Num>(), Ok(Num::Rat(-1, 2)));
        assert_eq!("+1/2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("#d1/2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("#d-1/2".parse::<Num>(), Ok(Num::Rat(-1, 2)));
        assert_eq!("#d+1/2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        // Accepted alternatives which may not be standard
        assert_eq!("1/+2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("+1/+2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("1/-2".parse::<Num>(), Ok(Num::Rat(-1, 2)));
        assert_eq!("-1/-2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("#d1/-2".parse::<Num>(), Ok(Num::Rat(-1, 2)));
        assert_eq!("#d-1/-2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("#d1/+2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        assert_eq!("#d+1/+2".parse::<Num>(), Ok(Num::Rat(1, 2)));
        // With exponent is error
        assert_eq!(
            "1/2e2".parse::<Num>(),
            Err(ArithErr::FailedParse("1/2e2".to_owned()))
        );
        assert_eq!(
            "1/2e-2".parse::<Num>(),
            Err(ArithErr::FailedParse("1/2e-2".to_owned()))
        );
        assert_eq!(
            "1e3/2".parse::<Num>(),
            Err(ArithErr::FailedParse("1e3/2".to_owned()))
        );
    }

    #[test]
    fn test_parsing_binary_integers() {
        // Simple
        assert_eq!("#b101".parse::<Num>(), Ok(Num::Int(5)));
        assert_eq!("#b-101".parse::<Num>(), Ok(Num::Int(-5)));
        assert_eq!("#b+101".parse::<Num>(), Ok(Num::Int(5)));
        // With exponent, /, . is error
        assert_eq!(
            "#b010e1".parse::<Num>(),
            Err(ArithErr::FailedParse("#b010e1".to_owned()))
        );
        assert_eq!(
            "#b010e-1".parse::<Num>(),
            Err(ArithErr::FailedParse("#b010e-1".to_owned()))
        );
        assert_eq!(
            "#b0/10".parse::<Num>(),
            Err(ArithErr::FailedParse("#b0/10".to_owned()))
        );
        assert_eq!(
            "#b0.10".parse::<Num>(),
            Err(ArithErr::FailedParse("#b0.10".to_owned()))
        );
        // Bad digit
        assert_eq!(
            "#b102".parse::<Num>(),
            Err(ArithErr::FailedParse("#b102".to_owned()))
        );
        // Without #b is decimal
        assert_eq!("1010".parse::<Num>(), Ok(Num::Int(1010)));
        assert_eq!("0010".parse::<Num>(), Ok(Num::Int(10)));
    }

    #[test]
    fn test_parsing_octal_integers() {
        // Simple
        assert_eq!("#o17".parse::<Num>(), Ok(Num::Int(15)));
        assert_eq!("#o-17".parse::<Num>(), Ok(Num::Int(-15)));
        assert_eq!("#o+17".parse::<Num>(), Ok(Num::Int(15)));
        // With exponent, /, . is error
        assert_eq!(
            "#o010e1".parse::<Num>(),
            Err(ArithErr::FailedParse("#o010e1".to_owned()))
        );
        assert_eq!(
            "#o010e-1".parse::<Num>(),
            Err(ArithErr::FailedParse("#o010e-1".to_owned()))
        );
        assert_eq!(
            "#o0/10".parse::<Num>(),
            Err(ArithErr::FailedParse("#o0/10".to_owned()))
        );
        assert_eq!(
            "#o0.10".parse::<Num>(),
            Err(ArithErr::FailedParse("#o0.10".to_owned()))
        );
        // Bad digit
        assert_eq!(
            "#o108".parse::<Num>(),
            Err(ArithErr::FailedParse("#o108".to_owned()))
        );
        // Without #o is decimal
        assert_eq!("1070".parse::<Num>(), Ok(Num::Int(1070)));
        assert_eq!("0070".parse::<Num>(), Ok(Num::Int(70)));
    }

    #[test]
    fn test_parsing_hex_integers() {
        // Simple
        assert_eq!("#xff".parse::<Num>(), Ok(Num::Int(255)));
        assert_eq!("#x-ff".parse::<Num>(), Ok(Num::Int(-255)));
        assert_eq!("#x+ff".parse::<Num>(), Ok(Num::Int(255)));
        // Positive exponent is ok
        assert_eq!("#x010e1".parse::<Num>(), Ok(Num::Int(4321)));
        // With negative exponent, /, . is error
        assert_eq!(
            "#x010e-2".parse::<Num>(),
            Err(ArithErr::FailedParse("#x010e-2".to_owned()))
        );
        assert_eq!(
            "#x0/10".parse::<Num>(),
            Err(ArithErr::FailedParse("#x0/10".to_owned()))
        );
        assert_eq!(
            "#x0.10".parse::<Num>(),
            Err(ArithErr::FailedParse("#x0.10".to_owned()))
        );
        // Bad digit
        assert_eq!(
            "#x10g".parse::<Num>(),
            Err(ArithErr::FailedParse("#x10g".to_owned()))
        );
        // Without #x is error
        assert_eq!(
            "a4f".parse::<Num>(),
            Err(ArithErr::FailedParse("a4f".to_owned()))
        );
        assert_eq!(
            "034b".parse::<Num>(),
            Err(ArithErr::FailedParse("034b".to_owned()))
        );
    }
}
