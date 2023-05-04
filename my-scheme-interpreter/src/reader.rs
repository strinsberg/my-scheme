use crate::heap::Heap;
use crate::scanner::Scanner;
use crate::scm_types::error::{ScmErr, ScmResult, ValResult};
use crate::scm_types::scm_val::ScmVal;
use crate::scm_types::token::Token;
use crate::utils;

// TODO confirm that the reader identifies all proper stopping points when
// parsing complex forms. I.e. a Dot in a vector is an error, is anything else?

pub struct StringReader<'a> {
    heap: &'a mut Heap,
    scanner: Scanner,
}

impl<'a> StringReader<'a> {
    pub fn new(string: &str, heap: &'a mut Heap) -> StringReader<'a> {
        StringReader {
            heap: heap,
            scanner: Scanner::new(string),
        }
    }

    pub fn read(&mut self) -> ValResult {
        let next = self.scanner.next()?;
        self.read_helper(next)
    }

    pub fn read_forms(&mut self) -> ScmResult<Vec<ScmVal>> {
        let mut forms = Vec::new();
        loop {
            let next = self.scanner.next()?;
            if next == Token::EOF {
                break;
            } else {
                forms.push(self.read_helper(next)?)
            }
        }
        Ok(forms)
    }

    fn read_helper(&mut self, token: Token) -> ValResult {
        match token {
            Token::Identifier(s) => Ok(ScmVal::Symbol(Box::new(s))),
            Token::Boolean(b) => Ok(ScmVal::Boolean(b)),
            Token::Number(num) => Ok(ScmVal::Number(num)),
            Token::Character(ch) => Ok(ScmVal::Character(ch)),
            Token::String(s) => utils::new_str_scm(s, self.heap),
            Token::LParen => self.read_list(),
            Token::VecOpen => self.read_vector(),
            Token::Quote => self.read_quote(),
            tk => Err(ScmErr::BadToken(self.scanner.line, tk)),
        }
    }

    fn read_list(&mut self) -> ValResult {
        // ( was used by caller
        let mut vec = Vec::new();
        let mut val = self.scanner.next()?;

        while val != Token::RParen {
            match val {
                Token::Dot => return self.read_dot_end(vec),
                _ => vec.push(self.read_helper(val)?),
            }
            val = self.scanner.next()?;
        }
        Ok(utils::vec_to_list(vec, self.heap))
    }

    fn read_dot_end(&mut self, values: Vec<ScmVal>) -> ValResult {
        // Dot was used by caller
        let next = self.scanner.next()?;
        match next {
            Token::RParen | Token::Dot | Token::EOF => {
                Err(ScmErr::BadToken(self.scanner.line, next.clone()))
            }
            _ => {
                // If next is a proper value check for ) and make the list
                let val = self.read_helper(next)?;
                match self.scanner.next()? {
                    Token::RParen => Ok(values
                        .into_iter()
                        .rev()
                        .fold(val, |acc, v| ScmVal::Pair(self.heap.cons(v.clone(), acc)))),
                    tk => Err(ScmErr::BadToken(self.scanner.line, tk)),
                }
            }
        }
    }

    fn read_quote(&mut self) -> ValResult {
        // really just makes a list with quote and the next form inside of it
        let next = self.read()?;
        Ok(utils::vec_to_list(
            vec![utils::new_sym("quote"), next],
            self.heap,
        ))
    }

    fn read_vector(&mut self) -> ValResult {
        // ( was used by caller
        let mut vec = Vec::new();
        let mut val = self.scanner.next()?;

        while val != Token::RParen {
            match val {
                Token::Dot => {
                    return Err(ScmErr::BadToken(self.scanner.line, val));
                }
                _ => vec.push(self.read_helper(val)?),
            }
            val = self.scanner.next()?;
        }
        Ok(ScmVal::Vector(vec))
    }
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scm_types::number::ScmNumber;
    use crate::utils;

    #[test]
    fn test_reading_values_parsed_by_the_scanner() {
        let mut h = Heap::new();
        assert_eq!(
            StringReader::new("10", &mut h).read(),
            Ok(utils::new_int(10))
        );
        assert_eq!(
            StringReader::new("1.234", &mut h).read(),
            Ok(utils::new_float(1.234))
        );
        assert_eq!(
            StringReader::new("#true", &mut h).read(),
            Ok(ScmVal::Boolean(true))
        );
        assert_eq!(
            StringReader::new("#f", &mut h).read(),
            Ok(ScmVal::Boolean(false))
        );
        assert_eq!(
            StringReader::new("#\\H", &mut h).read(),
            Ok(utils::new_char('H'))
        );
        assert_eq!(
            StringReader::new("#\\space", &mut h).read(),
            Ok(utils::new_char(' '))
        );
        assert_eq!(
            StringReader::new("hello-world!", &mut h).read(),
            Ok(utils::new_sym("hello-world!"))
        );
    }

    #[test]
    fn test_reading_strings() {
        let mut h = Heap::new();
        assert!(utils::str_eq_scm(
            "hello, world!",
            StringReader::new("\"hello, world!\"", &mut h)
                .read()
                .unwrap(),
            &h
        ));
        assert!(utils::str_eq_scm(
            "hello, world!",
            StringReader::new("\"hello, world!\"", &mut h)
                .read()
                .unwrap(),
            &h
        ));
    }

    #[test]
    fn test_reading_lists() {
        let mut h = Heap::new();

        // This is kind of brittle
        let expr = "(1 2 3 4)";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expr);

        let expr = "(1 2 3 . 4)";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expr);

        let expr = "(1 (2 3) . 4)";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expr);

        let expr = "(1 (2 3) . 4 5)";
        let result = StringReader::new(expr, &mut h).read();
        assert_eq!(
            result,
            Err(ScmErr::BadToken(1, Token::Number(ScmNumber::Integer(5))))
        );
    }

    #[test]
    fn test_reading_quote() {
        let mut h = Heap::new();

        let expr = "'(1 2 3 4)";
        let expect = "(quote (1 2 3 4))";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expect);

        let expr = "('1 2 '' 3 4)";
        let expect = "((quote 1) 2 (quote (quote 3)) 4)";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_reading_vectors() {
        let mut h = Heap::new();

        let expr = "#(1 '(2 3) 4)";
        let expect = "#(1 (quote (2 3)) 4)";
        let result = utils::val_to_string(StringReader::new(expr, &mut h).read().unwrap(), &h);
        assert_eq!(result, expect);

        let expr = "#(1 2 3 . 4)";
        let result = StringReader::new(expr, &mut h).read();
        assert_eq!(result, Err(ScmErr::BadToken(1, Token::Dot)));
    }

    #[test]
    fn test_reading_several_forms() {
        let mut h = Heap::new();

        let text = "1 #true\n'(1 2 3 4)\n\n(define a 5)";
        let result: Vec<String> = StringReader::new(text, &mut h)
            .read_forms()
            .unwrap()
            .into_iter()
            .map(|val| utils::val_to_string(val, &h))
            .collect();

        assert_eq!(result[0], "1".to_string());
        assert_eq!(result[1], "#t".to_string());
        assert_eq!(result[2], "(quote (1 2 3 4))".to_string());
        assert_eq!(result[3], "(define a 5)".to_string());
    }
}
