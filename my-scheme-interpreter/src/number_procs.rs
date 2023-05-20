use crate::err::Error;
use crate::number::Num;
use crate::value::Value;

// TODO add all number procs here.
// TODO not here, but put bool and symbol procs in the core procs or some other
// file since there are not many of them. That is where they will be called using
// a procedure object and a vector of arguments. The builtin array can be filled
// instead with procedure objects that record the name, formals, and types expected
// by the function. The vm can create meaningful user errors from the simple return
// errors using the procedure, arguments, and error. We may also be able to get
// rid of the builtin enum by creating the procedures like this and just match
// on the names, even if it is slower than matching on enums. Heck we may be
// able to put procedure versions in small vectors inside of each procs file and
// pull them together in the vm or core_procs. Would be even better if we could
// put a function in the procedure that took a vector of arguments and then we
// could just dispatch to that function from the procedure.
