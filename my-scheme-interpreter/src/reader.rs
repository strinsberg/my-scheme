use crate::error::{ScmErr, ScmResult, ValResult};
use crate::scanner::{Scanner, Token};
use crate::types::ScmVal;
use std::rc::Rc;

// TODO confirm that the reader identifies all proper stopping points when
// parsing complex forms. I.e. a Dot in a vector is an error, is anything else?

pub struct StringReader {
    scanner: Scanner,
}

impl StringReader {
    pub fn new(string: &str) -> StringReader {
        StringReader {
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
            Token::Identifier(s) => Ok(ScmVal::Symbol(Rc::new(s))),
            Token::Boolean(b) => Ok(ScmVal::Boolean(b)),
            Token::Number(num) => Ok(ScmVal::Number(num)),
            Token::Character(ch) => Ok(ScmVal::Character(ch)),
            Token::String(s) => Ok(ScmVal::new_str_mut_from_scmstring(s)),
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
        Ok(ScmVal::vec_to_list(vec, ScmVal::Empty))
    }

    fn read_dot_end(&mut self, values: Vec<ScmVal>) -> ValResult {
        // Dot was used by caller
        let next = self.scanner.next()?;
        match next {
            Token::RParen | Token::Dot | Token::EOF => {
                Err(ScmErr::BadToken(self.scanner.line, next.clone()))
            }
            Token::Quote => Err(ScmErr::Syntax(ScmVal::new_sym("quote"))),
            _ => {
                // If next is a proper value check for ) and make the list
                let val = self.read_helper(next)?;
                match self.scanner.next()? {
                    Token::RParen => Ok(ScmVal::vec_to_list(values, val)),
                    tk => Err(ScmErr::BadToken(self.scanner.line, tk)),
                }
            }
        }
    }

    fn read_quote(&mut self) -> ValResult {
        // really just makes a list with quote and the next form inside of it
        let next = self.read()?;
        Ok(ScmVal::new_pair(
            ScmVal::new_sym("quote"),
            ScmVal::new_pair(next, ScmVal::Empty),
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
        // TODO should vector literals be immutable???
        Ok(ScmVal::new_vec(vec))
    }
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number::ScmNumber;

    #[test]
    fn test_reading_values_parsed_by_the_scanner() {
        assert_eq!(StringReader::new("10").read(), Ok(ScmVal::new_int(10)));
        assert_eq!(
            StringReader::new("1.234").read(),
            Ok(ScmVal::new_float(1.234))
        );
        assert_eq!(StringReader::new("#true").read(), Ok(ScmVal::Boolean(true)));
        assert_eq!(StringReader::new("#f").read(), Ok(ScmVal::Boolean(false)));
        assert_eq!(StringReader::new("#\\H").read(), Ok(ScmVal::new_char('H')));
        assert_eq!(
            StringReader::new("#\\space").read(),
            Ok(ScmVal::new_char(' '))
        );
        assert_eq!(
            StringReader::new("hello-world!").read(),
            Ok(ScmVal::new_sym("hello-world!"))
        );
    }

    #[test]
    fn test_reading_strings() {
        assert_eq!(
            StringReader::new("\"hello, world!\"").read().unwrap(),
            ScmVal::new_str_mut("hello, world!"),
        );
    }

    #[test]
    fn test_reading_lists() {
        // This is kind of brittle
        let expr = "(1 2 3 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expr);

        let expr = "(1 2 3 . 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expr);

        let expr = "(1 (2 3) . 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expr);

        let expr = "(1 (2 3) . 4 5)";
        let result = StringReader::new(expr).read();
        assert_eq!(
            result,
            Err(ScmErr::BadToken(1, Token::Number(ScmNumber::Integer(5))))
        );
    }

    #[test]
    fn test_reading_quote() {
        let expr = "'(1 2 3 4)";
        let expect = "(quote (1 2 3 4))";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expect);

        let expr = "('1 2 '' 3 4)";
        let expect = "((quote 1) 2 (quote (quote 3)) 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_reading_vectors() {
        let expr = "#(1 2 3 4)";
        let expect = "#(1 2 3 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expect);

        let expr = "#(1 '(2 3) 4)";
        let expect = "#(1 (quote (2 3)) 4)";
        let result = StringReader::new(expr).read().unwrap().to_string();
        assert_eq!(result, expect);

        let expr = "#(1 2 3 . 4)";
        let result = StringReader::new(expr).read();
        assert_eq!(result, Err(ScmErr::BadToken(1, Token::Dot)));
    }

    #[test]
    fn test_reading_several_forms() {
        let text = "1 #true\n'(1 2 3 4)\n\n(define a 5)";
        let result: Vec<String> = StringReader::new(text)
            .read_forms()
            .unwrap()
            .into_iter()
            .map(|val| val.to_string())
            .collect();

        assert_eq!(result[0], "1".to_string());
        assert_eq!(result[1], "#t".to_string());
        assert_eq!(result[2], "(quote (1 2 3 4))".to_string());
        assert_eq!(result[3], "(define a 5)".to_string());
    }
}
