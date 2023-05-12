use crate::builtin::Builtin;
use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Closure, ConsCell, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

// Virtual Machine ////////////////////////////////////////////////////////////

type OpStackRc = Rc<Stack<VmOp>>;
type ResStackRc = Rc<Stack<ScmVal>>;
type EnvRc = Rc<RefCell<Env>>;

pub struct Vm {
    op_stack: OpStackRc,
    res_stack: ResStackRc,
    env: EnvRc,
}

impl Vm {
    pub fn new(env: Rc<RefCell<Env>>) -> Vm {
        Vm {
            op_stack: Stack::push(Stack::new_rc(VmOp::Halt, None), VmOp::Halt),
            res_stack: Stack::push(Stack::new_rc(ScmVal::Empty, None), ScmVal::Empty),
            env: env,
        }
    }

    pub fn eval(&mut self, expr: ScmVal) -> ValResult {
        self.init_stacks();
        self.op_stack = Stack::push(Rc::clone(&self.op_stack), VmOp::Eval(expr));

        loop {
            let op = self.pop_op();
            match op {
                VmOp::Eval(expr) => match expr.clone() {
                    ScmVal::Symbol(name) => {
                        let value = match self.env.borrow().lookup(expr) {
                            Some(val) => val,
                            None => return Err(ScmErr::Undeclared(name.to_string())),
                        };
                        self.push_res(value);
                    }
                    ScmVal::Pair(cell) => {
                        let (args_vec, dot, cycle) = ScmVal::list_to_vec(cell.tail.clone())
                            .ok_or(ScmErr::Syntax(expr.clone()))?;

                        match cell.head.clone() {
                            ScmVal::Symbol(name) => match name.to_string().as_str() {
                                "if" => {
                                    self.arity(args_vec.len(), 2, "if")?;
                                    self.push_op(VmOp::Apply(ScmVal::If(args_vec[1..].into())));
                                    self.push_op(VmOp::Eval(args_vec[0].clone()));
                                }
                                _ => return Err(ScmErr::Syntax(expr)),
                            },
                            _ => return Err(ScmErr::Syntax(expr)),
                        }
                    }
                    ScmVal::Vector(_) | ScmVal::VectorMut(_) | ScmVal::Cyclic => {}
                    _ => self.push_res(expr),
                },
                VmOp::Apply(val) => {
                    match val.clone() {
                        ScmVal::If(args) => {
                            let cond = self.pop_res();
                            match proc::is_true(cond) {
                                true => self.push_op(VmOp::Eval(args[0].clone())),
                                false => match args.len() {
                                    2.. => self.push_op(VmOp::Eval(args[1].clone())),
                                    _ => self.push_res(ScmVal::Empty),
                                },
                            }
                        }
                        _ => return Err(ScmErr::Syntax(val)),
                    }
                    // In here we take the value we have and apply it to the top
                    // level of arguments.
                    //
                    // If the application value was the special if form
                    // first we check res_stack.value to see if it is true/false
                    // pop the result stack
                    // then we put Eval(branch) onto the op_stack
                    //
                    // For a closure when you apply the procedure to it's arguments
                    // First push a SetEnv(current_env) onto the OpStack
                    // Then set the env to the captured env
                    // Then pop the number of args the closure needs and add
                    // them to the env for the right params
                    // Then we need to eval and discard all the body expressions
                    // in order. The last has to be tco, which it will be because
                    // all argument eval and body eval will be off of the stack
                    // by the time we get to the final body expression.
                }
                VmOp::SetOpStack(val) => {}
                VmOp::SetResStack(val) => {}
                VmOp::SetEnv(env) => {}
                VmOp::Discard => break,
                VmOp::Halt => break,
            }
        }

        Ok(self.res_stack.value.clone())
    }

    /*** Stack Helpers ***/
    fn init_stacks(&mut self) {
        self.op_stack = Stack::push(Stack::new_rc(VmOp::Halt, None), VmOp::Halt);
        self.res_stack = Stack::push(Stack::new_rc(ScmVal::Empty, None), ScmVal::Empty);
    }

    fn pop_op(&mut self) -> VmOp {
        match self.op_stack.next.clone() {
            Some(stack) => {
                let val = self.op_stack.value.clone();
                self.op_stack = stack;
                val
            }
            None => panic!("popped an empty op stack"),
        }
    }

    fn pop_res(&mut self) -> ScmVal {
        match self.res_stack.next.clone() {
            Some(stack) => {
                let val = self.res_stack.value.clone();
                self.res_stack = stack;
                val
            }
            None => panic!("popped an empty res stack"),
        }
    }

    fn push_op(&mut self, op: VmOp) {
        self.op_stack = Stack::push(Rc::clone(&self.op_stack), op);
    }

    fn push_res(&mut self, val: ScmVal) {
        self.res_stack = Stack::push(Rc::clone(&self.res_stack), val);
    }

    /*** Error Helpers ***/

    fn arity(&self, len: usize, min: usize, name: &str) -> Result<(), ScmErr> {
        if len < min {
            Err(ScmErr::Arity(name.to_owned(), min))
        } else {
            Ok(())
        }
    }
}

// Vm Operations //////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
enum VmOp {
    Eval(ScmVal),
    Apply(ScmVal),
    SetOpStack(EnvRc),
    SetResStack(EnvRc),
    SetEnv(EnvRc),
    Discard,
    Halt,
}

// Stack //////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Stack<T> {
    pub value: T,
    pub next: Option<Rc<Stack<T>>>,
}

impl<T> Stack<T>
where
    T: Clone,
{
    pub fn new(value: T, next: Option<Rc<Stack<T>>>) -> Stack<T> {
        Stack {
            value: value,
            next: next,
        }
    }

    pub fn new_rc(value: T, next: Option<Rc<Stack<T>>>) -> Rc<Stack<T>> {
        Rc::new(Stack {
            value: value,
            next: next,
        })
    }

    pub fn push(stack: Rc<Stack<T>>, value: T) -> Rc<Stack<T>> {
        Stack::new_rc(value, Some(stack))
    }

    pub fn update(stack: Rc<Stack<T>>, value: T) -> Rc<Stack<T>> {
        Stack::new_rc(value, stack.next.clone())
    }
}

// Testing ////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::StringReader;

    #[test]
    fn test_eval_symbol() {
        let env = Rc::new(RefCell::new(
            Env::new_with_bindings(vec![(ScmVal::new_sym("A"), ScmVal::new_int(5))]).unwrap(),
        ));
        let mut vm = Vm::new(Rc::clone(&env));
        assert_eq!(vm.eval(ScmVal::new_sym("A")), Ok(ScmVal::new_int(5)));
        assert_eq!(
            vm.eval(ScmVal::new_sym("B")),
            Err(ScmErr::Undeclared("B".to_owned()))
        );
    }

    #[test]
    fn test_eval_simple_if() {
        let env = Rc::new(RefCell::new(Env::new()));
        let mut vm = Vm::new(Rc::clone(&env));

        let if_stmt = StringReader::new("(if #t 1 0)").read().unwrap();
        assert_eq!(vm.eval(if_stmt), Ok(ScmVal::new_int(1)));

        let if_stmt = StringReader::new("(if #f 1 0)").read().unwrap();
        assert_eq!(vm.eval(if_stmt), Ok(ScmVal::new_int(0)));

        let if_stmt = StringReader::new("(if #t 1)").read().unwrap();
        assert_eq!(vm.eval(if_stmt), Ok(ScmVal::new_int(1)));

        let if_stmt = StringReader::new("(if #f 1)").read().unwrap();
        assert_eq!(vm.eval(if_stmt), Ok(ScmVal::Empty));
    }
}
