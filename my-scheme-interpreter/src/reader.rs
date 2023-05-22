use crate::array::Array;
use crate::cell::Cell;
use crate::err::ScanError;
use crate::scanner::{Scanner, Token};
use crate::string::Str;
use crate::value::Value;

// TODO confirm that the reader identifies all proper stopping points when
// parsing complex forms. I.e. a Dot in a vector is an error, is anything else?
//
// TODO for read and write to work in the expected way they will have to use
// i/o object streams and not strings. As far as I know there is probably a rust
// object to hold a string as an i/o object so if the reader and scanner hold
// an i/o object there can be objects/functions that use them, but with a string
// to get the same effect as we have now. Makes more sense than rewriting the
// logic in here (worse for the scanner) just to have an i/o object.

#[derive(Debug)]
pub struct StringReader {
    scanner: Scanner,
}

impl StringReader {
    pub fn new(string: &str) -> StringReader {
        StringReader {
            scanner: Scanner::new(string),
        }
    }

    pub fn read(&mut self) -> Result<Value, ScanError> {
        let next = self.scanner.next()?;
        self.read_helper(next)
    }

    pub fn read_forms(&mut self) -> Result<Vec<Value>, ScanError> {
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

    fn read_helper(&mut self, token: Token) -> Result<Value, ScanError> {
        match token {
            Token::Identifier(s) => Ok(Value::symbol(s)),
            Token::Boolean(b) => Ok(Value::Bool(b)),
            Token::Number(num) => Ok(Value::Number(num)),
            Token::Character(ch) => Ok(Value::Char(ch)),
            Token::String(s) => Ok(Value::from(s)),
            Token::LParen => self.read_list(),
            Token::VecOpen => self.read_vector(),
            Token::Quote => self.read_quote(),
            tk => Err(ScanError::BadToken(self.scanner.line, tk.to_string())),
        }
    }

    fn read_list(&mut self) -> Result<Value, ScanError> {
        // ( was used by caller
        let mut vec = Vec::new();
        let mut val = self.scanner.next()?;

        while val != Token::RParen {
            match val {
                Token::Dot => return self.read_dot_end(&vec),
                _ => vec.push(self.read_helper(val)?),
            }
            val = self.scanner.next()?;
        }
        Ok(Value::list_from_vec(vec, Value::Empty))
    }

    fn read_dot_end(&mut self, values: &[Value]) -> Result<Value, ScanError> {
        // Dot was used by caller
        let next = self.scanner.next()?;
        match next {
            Token::RParen | Token::Dot | Token::EOF => {
                Err(ScanError::BadToken(self.scanner.line, next.to_string()))
            }
            Token::Quote => Err(ScanError::BadToken(
                self.scanner.line,
                Token::Quote.to_string(),
            )),
            _ => {
                // If next is a proper value check for ) and make the list
                let val = self.read_helper(next)?;
                match self.scanner.next()? {
                    Token::RParen => Ok(Value::list_from_vec(values.into(), val)),
                    tk => Err(ScanError::BadToken(self.scanner.line, tk.to_string())),
                }
            }
        }
    }

    fn read_quote(&mut self) -> Result<Value, ScanError> {
        // really just makes a list with quote and the next form inside of it
        let next = self.read()?;
        Ok(Value::from(Cell::new(
            Value::symbol(Str::from("quote")),
            Some(Value::from(Cell::new(next, None))),
        )))
    }

    fn read_vector(&mut self) -> Result<Value, ScanError> {
        // ( was used by caller
        let mut vec = Vec::new();
        let mut val = self.scanner.next()?;

        while val != Token::RParen {
            match val {
                Token::Dot => {
                    return Err(ScanError::BadToken(self.scanner.line, val.to_string()));
                }
                _ => vec.push(self.read_helper(val)?),
            }
            val = self.scanner.next()?;
        }
        Ok(Value::from(Array::from(vec)))
    }
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number::Num;

    #[test]
    fn test_reading_values_parsed_by_the_scanner() {
        assert_eq!(StringReader::new("10").read(), Ok(Value::from(10)));
        assert_eq!(StringReader::new("1.234").read(), Ok(Value::from(1.234)));
        assert_eq!(StringReader::new("#true").read(), Ok(Value::Bool(true)));
        assert_eq!(StringReader::new("#f").read(), Ok(Value::Bool(false)));
        assert_eq!(StringReader::new("#\\H").read(), Ok(Value::from('H')));
        assert_eq!(StringReader::new("#\\space").read(), Ok(Value::from(' ')));
        assert_eq!(
            StringReader::new("hello-world!").read(),
            Ok(Value::symbol(Str::from("hello-world!")))
        );
    }

    #[test]
    fn test_reading_strings() {
        assert_eq!(
            StringReader::new("\"hello, world!\"").read().unwrap(),
            Value::from("hello, world!"),
        );
    }

    #[test]
    fn test_reading_lists() {
        // This is kind of brittle
        println!("help");
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
            Err(ScanError::BadToken(
                1,
                Token::Number(Num::Int(5)).to_string()
            ))
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
        assert_eq!(result, Err(ScanError::BadToken(1, Token::Dot.to_string())));
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
