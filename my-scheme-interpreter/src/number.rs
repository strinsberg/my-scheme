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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ScmNumber {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
}

impl ScmNumber {
    pub fn new_rational(a: i64, b: i64) -> Option<ScmNumber> {
        // denominator cannot be 0, if denom is negative swap the sign with numer,
        // if both are negative make them both positive.
        if b == 0 {
            None
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
                ScmNumber::simplify_fraction((a * 1) + (b * i), b * 1)
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
                ScmNumber::simplify_fraction((a * 1) + (b * i), b * 1)
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
        ScmNumber::new_rational(n / m, d / m)
    }

    pub fn is_zero(&self) -> bool {
        match *self {
            ScmNumber::Integer(i) => i == 0,
            ScmNumber::Rational(a, _) => a == 0,
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

    fn _is_int(&self) -> bool {
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
