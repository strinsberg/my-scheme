use std::cmp::Ordering;
use std::fmt;

// TODO add necessary arithmetic functions from the report that are needed
// to implement others in core_proc or in scheme code.

// Arithmetic Errors //////////////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub enum ArithErr {
    DivideByZero,
    FailedParse(String),
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

    // This should be used everywhere a user can create a rational number. It
    // does the necessary checks and simplifications to ensure they are always
    // in a consistent state.
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
            (Num::Flt(_), _) | (_, Num::Flt(_)) => Ok(Num::Flt(self.as_f64() + right.as_f64())),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify((a * d) + (b * c), b * d),
            (Num::Rat(a, b), Num::Int(i)) | (Num::Int(i), Num::Rat(a, b)) => {
                Num::simplify(a + (b * i), *b)
            }
            (_, _) => Ok(Num::Int(self.as_i64() + right.as_i64())),
        }
    }

    pub fn sub(&self, right: &Num) -> Result<Num, ArithErr> {
        match (self, right) {
            (Num::Flt(_), _) | (_, Num::Flt(_)) => Ok(Num::Flt(self.as_f64() - right.as_f64())),
            (Num::Rat(a, b), Num::Rat(c, d)) => Num::simplify((a * d) - (b * c), b * d),
            (Num::Rat(a, b), Num::Int(i)) => Num::simplify(a - (b * i), *b),
            (Num::Int(i), Num::Rat(a, b)) => Num::simplify((b * i) - a, *b),
            (_, _) => Ok(Num::Int(self.as_i64() - right.as_i64())),
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
            (Num::Rat(a, b), Num::Int(i)) => Num::simplify(*a, b * i),
            (Num::Int(i), Num::Rat(a, b)) => Num::simplify(b * i, *a),
            (_, _) => Ok(Num::Flt(self.as_f64() / right.as_f64())),
        }
    }

    pub fn negate(&self) -> Num {
        match self {
            Num::Int(i) => Num::Int(-i),
            Num::Flt(f) => Num::Flt(-f),
            Num::Rat(a, b) => match self.is_positive() {
                true => Num::new_rat(-a, *b).expect("rational should not be in invalid state"),
                false => {
                    Num::new_rat(abs(*a), abs(*b)).expect("rational should not be in invalid state")
                }
            },
        }
    }

    pub fn invert(&self) -> Num {
        match self {
            Num::Int(i) => match self.is_zero() {
                true => Num::Int(0),
                false => Num::Rat(1, *i),
            },
            Num::Flt(f) => match self.is_zero() {
                true => Num::Flt(0.0),
                false => Num::Flt(1.0 / f),
            },
            Num::Rat(a, b) => match self.is_zero() {
                true => Num::Int(0),
                false => Num::Rat(*b, *a),
            },
        }
    }

    fn simplify(n: i64, d: i64) -> Result<Num, ArithErr> {
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

    pub fn is_negative(&self) -> bool {
        if self.is_zero() {
            false
        } else {
            !self.is_positive()
        }
    }

    pub fn is_positive(&self) -> bool {
        if self.is_zero() {
            false
        } else {
            match *self {
                Num::Int(i) => i > 0,
                Num::Flt(f) => f > 0.0,
                Num::Rat(a, b) => (a > 0 && b > 0) || (b < 0 && a < 0),
            }
        }
    }

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
                let x = Num::simplify(*a, *b).expect("rational should not be in invalid state");
                match x {
                    Num::Int(y) => *i == y,
                    _ => false,
                }
            }
            (Num::Int(x), Num::Int(y)) => x == y,
        }
    }
}

impl Eq for Num {}

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

// Other numeric functions ////////////////////////////////////////////////////

fn abs(a: i64) -> i64 {
    if a < 0 {
        -a
    } else {
        a
    }
}

// Supposed to be an efficient gcd, copied from wikipedia, uh oh.
// https://en.wikipedia.org/wiki/Binary_GCD_algorithm
fn gcd(mut u: u64, mut v: u64) -> u64 {
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

// Testing ////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;

    // Aritmetic //

    #[test]
    fn test_integer_arithmetic() {
        assert_eq!(Num::Int(5).add(&Num::Int(10)), Ok(Num::Int(15)));
        assert_eq!(Num::Int(5).sub(&Num::Int(10)), Ok(Num::Int(-5)));
        assert_eq!(Num::Int(5).mult(&Num::Int(10)), Ok(Num::Int(50)));
        assert_eq!(Num::Int(5).div(&Num::Int(10)), Ok(Num::Rat(1, 2)));

        assert_eq!(Num::Int(-5).add(&Num::Int(10)), Ok(Num::Int(5)));
        assert_eq!(Num::Int(-5).sub(&Num::Int(10)), Ok(Num::Int(-15)));
        assert_eq!(Num::Int(-5).mult(&Num::Int(10)), Ok(Num::Int(-50)));
        assert_eq!(Num::Int(-5).div(&Num::Int(10)), Ok(Num::Rat(-1, 2)));

        assert_eq!(Num::Int(5).add(&Num::Int(-10)), Ok(Num::Int(-5)));
        assert_eq!(Num::Int(5).sub(&Num::Int(-10)), Ok(Num::Int(15)));
        assert_eq!(Num::Int(5).mult(&Num::Int(-10)), Ok(Num::Int(-50)));
        assert_eq!(Num::Int(5).div(&Num::Int(-10)), Ok(Num::Rat(-1, 2)));

        assert_eq!(Num::Int(-5).add(&Num::Int(-10)), Ok(Num::Int(-15)));
        assert_eq!(Num::Int(-5).sub(&Num::Int(-10)), Ok(Num::Int(5)));
        assert_eq!(Num::Int(-5).mult(&Num::Int(-10)), Ok(Num::Int(50)));
        assert_eq!(Num::Int(-5).div(&Num::Int(-10)), Ok(Num::Rat(1, 2)));

        assert_eq!(Num::Int(5).negate(), Num::Int(-5));
        assert_eq!(Num::Int(-5).negate(), Num::Int(5));
        assert_eq!(Num::Int(0).negate(), Num::Int(0));

        assert_eq!(Num::Int(5).invert(), Num::Rat(1, 5));
        assert_eq!(Num::Int(-5).invert(), Num::Rat(-1, 5));
        assert_eq!(Num::Int(0).invert(), Num::Int(0));
    }

    #[test]
    fn test_float_arithmetic() {
        assert_eq!(Num::Flt(5.0).add(&Num::Flt(10.0)), Ok(Num::Flt(15.0)));
        assert_eq!(Num::Flt(5.0).sub(&Num::Flt(10.0)), Ok(Num::Flt(-5.0)));
        assert_eq!(Num::Flt(5.0).mult(&Num::Flt(10.0)), Ok(Num::Flt(50.0)));
        assert_eq!(Num::Flt(5.0).div(&Num::Flt(10.0)), Ok(Num::Flt(0.5)));

        assert_eq!(Num::Flt(-5.0).add(&Num::Flt(10.0)), Ok(Num::Flt(5.0)));
        assert_eq!(Num::Flt(-5.0).sub(&Num::Flt(10.0)), Ok(Num::Flt(-15.0)));
        assert_eq!(Num::Flt(-5.0).mult(&Num::Flt(10.0)), Ok(Num::Flt(-50.0)));
        assert_eq!(Num::Flt(-5.0).div(&Num::Flt(10.0)), Ok(Num::Flt(-0.5)));

        assert_eq!(Num::Flt(5.0).add(&Num::Flt(-10.0)), Ok(Num::Flt(-5.0)));
        assert_eq!(Num::Flt(5.0).sub(&Num::Flt(-10.0)), Ok(Num::Flt(15.0)));
        assert_eq!(Num::Flt(5.0).mult(&Num::Flt(-10.0)), Ok(Num::Flt(-50.0)));
        assert_eq!(Num::Flt(5.0).div(&Num::Flt(-10.0)), Ok(Num::Flt(-0.5)));

        assert_eq!(Num::Flt(-5.0).add(&Num::Flt(-10.0)), Ok(Num::Flt(-15.0)));
        assert_eq!(Num::Flt(-5.0).sub(&Num::Flt(-10.0)), Ok(Num::Flt(5.0)));
        assert_eq!(Num::Flt(-5.0).mult(&Num::Flt(-10.0)), Ok(Num::Flt(50.0)));
        assert_eq!(Num::Flt(-5.0).div(&Num::Flt(-10.0)), Ok(Num::Flt(0.5)));

        assert_eq!(Num::Flt(5.0).negate(), Num::Flt(-5.0));
        assert_eq!(Num::Flt(-5.0).negate(), Num::Flt(5.0));
        assert_eq!(Num::Flt(0.0).negate(), Num::Flt(0.0));

        assert_eq!(Num::Flt(5.0).invert(), Num::Flt(0.2));
        assert_eq!(Num::Flt(-5.0).invert(), Num::Flt(-0.2));
        assert_eq!(Num::Flt(0.0).invert(), Num::Flt(0.0));
    }

    #[test]
    fn test_rational_arithmetic() {
        assert_eq!(Num::Rat(2, 5).add(&Num::Rat(2, 8)), Ok(Num::Rat(13, 20)));
        assert_eq!(Num::Rat(2, 5).sub(&Num::Rat(2, 8)), Ok(Num::Rat(3, 20)));
        assert_eq!(Num::Rat(2, 5).mult(&Num::Rat(2, 8)), Ok(Num::Rat(1, 10)));
        assert_eq!(Num::Rat(2, 5).div(&Num::Rat(2, 8)), Ok(Num::Rat(8, 5)));

        assert_eq!(Num::Rat(-2, 5).add(&Num::Rat(2, 8)), Ok(Num::Rat(-3, 20)));
        assert_eq!(Num::Rat(-2, 5).sub(&Num::Rat(2, 8)), Ok(Num::Rat(-13, 20)));
        assert_eq!(Num::Rat(-2, 5).mult(&Num::Rat(2, 8)), Ok(Num::Rat(-1, 10)));
        assert_eq!(Num::Rat(-2, 5).div(&Num::Rat(2, 8)), Ok(Num::Rat(-8, 5)));

        assert_eq!(Num::Rat(2, 5).add(&Num::Rat(-2, 8)), Ok(Num::Rat(3, 20)));
        assert_eq!(Num::Rat(2, 5).sub(&Num::Rat(-2, 8)), Ok(Num::Rat(13, 20)));
        assert_eq!(Num::Rat(2, 5).mult(&Num::Rat(-2, 8)), Ok(Num::Rat(-1, 10)));
        assert_eq!(Num::Rat(2, 5).div(&Num::Rat(-2, 8)), Ok(Num::Rat(-8, 5)));

        assert_eq!(Num::Rat(-2, 5).add(&Num::Rat(-2, 8)), Ok(Num::Rat(-13, 20)));
        assert_eq!(Num::Rat(-2, 5).sub(&Num::Rat(-2, 8)), Ok(Num::Rat(-3, 20)));
        assert_eq!(Num::Rat(-2, 5).mult(&Num::Rat(-2, 8)), Ok(Num::Rat(1, 10)));
        assert_eq!(Num::Rat(-2, 5).div(&Num::Rat(-2, 8)), Ok(Num::Rat(8, 5)));

        assert_eq!(Num::Rat(2, 5).negate(), Num::Rat(-2, 5));
        assert_eq!(Num::Rat(-2, 5).negate(), Num::Rat(2, 5));
        assert_eq!(Num::Rat(-2, -5).negate(), Num::Rat(-2, 5));
        assert_eq!(Num::Rat(0, 1).negate(), Num::Int(0));

        assert_eq!(Num::Rat(2, 5).invert(), Num::Rat(5, 2));
        assert_eq!(Num::Rat(-2, 5).invert(), Num::Rat(5, -2));
        assert_eq!(Num::Rat(-2, -5).invert(), Num::Rat(-5, -2));
        assert_eq!(Num::Rat(0, 1).invert(), Num::Int(0));
    }

    #[test]
    fn test_mixed_arithmetic() {
        assert_eq!(Num::Int(5).add(&Num::Flt(10.0)), Ok(Num::Flt(15.0)));
        assert_eq!(Num::Int(5).sub(&Num::Flt(10.0)), Ok(Num::Flt(-5.0)));
        assert_eq!(Num::Int(5).mult(&Num::Flt(10.0)), Ok(Num::Flt(50.0)));
        assert_eq!(Num::Int(5).div(&Num::Flt(10.0)), Ok(Num::Flt(0.5)));

        assert_eq!(Num::Int(5).add(&Num::Rat(1, 2)), Ok(Num::Rat(11, 2)));
        assert_eq!(Num::Int(5).sub(&Num::Rat(1, 2)), Ok(Num::Rat(9, 2)));
        assert_eq!(Num::Int(5).mult(&Num::Rat(1, 2)), Ok(Num::Rat(5, 2)));
        assert_eq!(Num::Int(5).div(&Num::Rat(1, 2)), Ok(Num::Int(10)));
        // order matters for rational sub and divide with int
        assert_eq!(Num::Rat(1, 2).sub(&Num::Int(5)), Ok(Num::Rat(-9, 2)));
        assert_eq!(Num::Rat(1, 2).div(&Num::Int(5)), Ok(Num::Rat(1, 10)));

        assert_eq!(Num::Flt(5.0).add(&Num::Rat(1, 2)), Ok(Num::Flt(5.5)));
        assert_eq!(Num::Flt(5.0).sub(&Num::Rat(1, 2)), Ok(Num::Flt(4.5)));
        assert_eq!(Num::Flt(5.0).mult(&Num::Rat(1, 2)), Ok(Num::Flt(2.5)));
        assert_eq!(Num::Flt(5.0).div(&Num::Rat(1, 2)), Ok(Num::Flt(10.0)));
    }

    // Predicates //

    #[test]
    fn test_is_positive() {
        assert_eq!(Num::Flt(5.0).is_positive(), true);
        assert_eq!(Num::Flt(-5.0).is_positive(), false);
        assert_eq!(Num::Flt(0.0).is_positive(), false);

        assert_eq!(Num::Int(5).is_positive(), true);
        assert_eq!(Num::Int(-5).is_positive(), false);
        assert_eq!(Num::Int(0).is_positive(), false);

        assert_eq!(Num::Rat(5, 3).is_positive(), true);
        assert_eq!(Num::Rat(-5, -3).is_positive(), true);
        assert_eq!(Num::Rat(-5, 3).is_positive(), false);
        assert_eq!(Num::Rat(5, -3).is_positive(), false);
        assert_eq!(Num::Rat(0, 1).is_positive(), false);
    }

    #[test]
    fn test_is_negative() {
        assert_eq!(Num::Flt(5.0).is_negative(), false);
        assert_eq!(Num::Flt(-5.0).is_negative(), true);
        assert_eq!(Num::Flt(0.0).is_negative(), false);

        assert_eq!(Num::Int(5).is_negative(), false);
        assert_eq!(Num::Int(-5).is_negative(), true);
        assert_eq!(Num::Int(0).is_negative(), false);

        assert_eq!(Num::Rat(5, 3).is_negative(), false);
        assert_eq!(Num::Rat(-5, -3).is_negative(), false);
        assert_eq!(Num::Rat(-5, 3).is_negative(), true);
        assert_eq!(Num::Rat(5, -3).is_negative(), true);
        assert_eq!(Num::Rat(0, 1).is_negative(), false);
    }

    #[test]
    fn test_is_zero() {
        assert_eq!(Num::Flt(5.0).is_zero(), false);
        assert_eq!(Num::Flt(-5.0).is_zero(), false);
        assert_eq!(Num::Flt(0.0).is_zero(), true);

        assert_eq!(Num::Int(5).is_zero(), false);
        assert_eq!(Num::Int(-5).is_zero(), false);
        assert_eq!(Num::Int(0).is_zero(), true);

        assert_eq!(Num::Rat(5, 3).is_zero(), false);
        assert_eq!(Num::Rat(-5, -3).is_zero(), false);
        assert_eq!(Num::Rat(-5, 3).is_zero(), false);
        assert_eq!(Num::Rat(5, -3).is_zero(), false);
        assert_eq!(Num::Rat(0, 1).is_zero(), true);
        assert_eq!(Num::Rat(0, 30).is_zero(), true);
    }

    #[test]
    fn test_is_type_check() {
        assert_eq!(Num::Flt(5.0).is_flt(), true);
        assert_eq!(Num::Int(5).is_flt(), false);
        assert_eq!(Num::Rat(5, 8).is_flt(), false);

        assert_eq!(Num::Flt(5.0).is_int(), false);
        assert_eq!(Num::Int(5).is_int(), true);
        assert_eq!(Num::Rat(5, 8).is_int(), false);

        assert_eq!(Num::Flt(5.0).is_rat(), false);
        assert_eq!(Num::Int(5).is_rat(), false);
        assert_eq!(Num::Rat(5, 8).is_rat(), true);
    }

    // Parsing //

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

    // Comparisson //

    #[test]
    fn test_equality() {
        assert!(Num::Int(5) == Num::Int(5));
        assert!(Num::Int(4) != Num::Int(5));

        assert!(Num::Flt(5.0) == Num::Flt(5.0));
        assert!(Num::Flt(5.1) != Num::Flt(5.0));

        assert!(Num::Rat(1, 5) == Num::Rat(1, 5));
        assert!(Num::Rat(1, 5) == Num::Rat(2, 10));
        assert!(Num::Rat(2, 10) == Num::Rat(4, 20));
        assert!(Num::Rat(1, 4) != Num::Rat(1, 5));

        assert!(Num::Int(5) == Num::Rat(5, 1));
        assert!(Num::Int(5) == Num::Flt(5.0));

        assert!(Num::Rat(5, 1) == Num::Flt(5.0));
        assert!(Num::Rat(1, 2) == Num::Flt(0.5));
    }

    #[test]
    fn test_compare_integers() {
        assert!(Num::Int(5) <= Num::Int(5));
        assert!(Num::Int(4) <= Num::Int(5));
        assert!(Num::Int(4) < Num::Int(5));
        assert!(Num::Int(5) >= Num::Int(5));
        assert!(Num::Int(5) >= Num::Int(4));
        assert!(Num::Int(5) > Num::Int(4));
    }

    #[test]
    fn test_compare_float() {
        assert!(Num::Flt(5.0) <= Num::Flt(5.0));
        assert!(Num::Flt(4.0) <= Num::Flt(5.0));
        assert!(Num::Flt(4.0) < Num::Flt(5.0));
        assert!(Num::Flt(5.0) >= Num::Flt(5.0));
        assert!(Num::Flt(5.0) >= Num::Flt(4.0));
        assert!(Num::Flt(5.0) > Num::Flt(4.0));
    }

    #[test]
    fn test_compare_rationals() {
        assert!(Num::Rat(5, 6) <= Num::Rat(5, 6));
        assert!(Num::Rat(2, 6) <= Num::Rat(5, 6));
        assert!(Num::Rat(2, 6) < Num::Rat(5, 6));
        assert!(Num::Rat(5, 6) >= Num::Rat(5, 6));
        assert!(Num::Rat(5, 6) >= Num::Rat(2, 6));
        assert!(Num::Rat(5, 6) > Num::Rat(2, 6));
    }

    #[test]
    fn test_compare_mixed() {
        assert!(Num::Rat(6, 6) <= Num::Int(1));
        assert!(Num::Rat(2, 6) <= Num::Int(1));
        assert!(Num::Rat(2, 6) < Num::Int(1));

        assert!(Num::Rat(6, 6) >= Num::Int(1));
        assert!(Num::Rat(8, 6) >= Num::Int(1));
        assert!(Num::Rat(8, 6) > Num::Int(1));

        assert!(Num::Rat(1, 2) <= Num::Flt(0.5));
        assert!(Num::Rat(1, 4) <= Num::Flt(0.5));
        assert!(Num::Rat(1, 4) < Num::Flt(0.5));

        assert!(Num::Rat(1, 2) >= Num::Flt(0.5));
        assert!(Num::Rat(3, 4) >= Num::Flt(0.5));
        assert!(Num::Rat(3, 4) > Num::Flt(0.5));

        assert!(Num::Int(1) <= Num::Flt(1.0));
        assert!(Num::Int(1) <= Num::Flt(1.5));
        assert!(Num::Int(1) < Num::Flt(1.5));

        assert!(Num::Int(1) >= Num::Flt(1.0));
        assert!(Num::Int(3) >= Num::Flt(1.5));
        assert!(Num::Int(3) > Num::Flt(1.5));
    }

    // String Out //

    #[test]
    fn test_number_to_string() {
        assert_eq!(Num::Rat(6, 6).to_string(), "6/6".to_string());
        assert_eq!(Num::Rat(-6, 6).to_string(), "-6/6".to_string());
        assert_eq!(Num::Rat(6, -6).to_string(), "6/-6".to_string());
        assert_eq!(Num::Rat(-6, -6).to_string(), "-6/-6".to_string());

        assert_eq!(Num::Int(4).to_string(), "4".to_string());
        assert_eq!(Num::Int(-4).to_string(), "-4".to_string());

        assert_eq!(Num::Flt(4.234).to_string(), "4.234".to_string());
        assert_eq!(Num::Flt(-4.234).to_string(), "-4.234".to_string());

        assert_eq!(
            Num::Flt(0.000000000234).to_string(),
            "0.000000000234".to_string()
        );
        assert_eq!(
            Num::Flt(-0.000000000234).to_string(),
            "-0.000000000234".to_string()
        );
        assert_eq!(
            Num::Flt(4.234e20).to_string(),
            "423400000000000000000".to_string()
        );
        assert_eq!(
            Num::Flt(-4.234e20).to_string(),
            "-423400000000000000000".to_string()
        );

        assert_eq!(
            Num::Int(4000000000000000).to_string(),
            "4000000000000000".to_string()
        );
    }
}
