use crate::data::err::Error;
use crate::data::value::Value;
use crate::proc::numbers;

// TODO add some more common functions, make sure to add arity checking
// since the argument list must be large enough to provide the parameters
// to the wrapped function unless it is a collect.

pub fn add(args: Vec<Value>) -> Result<Value, Error> {
    numbers::add(Value::list_from_vec(args, Value::Empty))
}
