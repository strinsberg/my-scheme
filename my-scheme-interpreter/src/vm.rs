use crate::core_proc as proc;
use crate::error::{ScmErr, ValResult};
use crate::eval_helpers as evh;
use crate::types::{Closure, ConsCell, Env, ScmVal, SpecialForm};
use std::cell::RefCell;
use std::rc::Rc;

// TODO since all of the action in the eval function is accessing the vm internals
// it should be quite easy to move all of the bigger sections to their own methods
// without the same shenanigans that we had to use last time.
// TODO add derived expressions. They may need to be either re-written as
// transformations or made into special forms.
// TODO add an eval for multiple forms.
// TODO Get rid of mutable pairs and use the mutable flag in the ConsCell.
// TODO replace the interpreters evaluator with this one and run the full tests suite.
// TODO implement a continuation

type OpStackRc = Rc<Stack<VmOp>>;
type ResStackRc = Rc<Stack<ScmVal>>;
type EnvRc = Rc<RefCell<Env>>;

// Virtual Machine ////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
enum VmOp {
    Eval(ScmVal),
    Apply(ScmVal, usize),
    SetOpStack(EnvRc),
    SetResStack(EnvRc),
    SetEnv(EnvRc),
    ApplyRes(usize),
    Discard,
    Halt,
}

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

    // Evaluate a single form
    pub fn eval(&mut self, expr: ScmVal) -> ValResult {
        self.init_stacks();
        self.op_stack = Stack::push(Rc::clone(&self.op_stack), VmOp::Eval(expr));

        loop {
            let op = self.pop_op();
            match op {
                VmOp::Eval(expr) => match expr.clone() {
                    ScmVal::Symbol(name) => self.eval_symbol(expr, &name.to_string())?,
                    ScmVal::Pair(cell) => self.eval_pair(cell)?,
                    ScmVal::Vector(_) | ScmVal::VectorMut(_) | ScmVal::Cyclic => {
                        return Err(ScmErr::Syntax(expr))
                    }
                    _ => self.push_res(expr),
                },
                VmOp::Apply(val, n) => self.apply(val, n)?,
                VmOp::SetOpStack(val) => {}
                VmOp::SetResStack(val) => {}
                VmOp::SetEnv(env) => {
                    self.env = env;
                }
                VmOp::ApplyRes(n) => {
                    let proc = self.pop_res();
                    self.push_op(VmOp::Apply(proc, n));
                }
                VmOp::Discard => {
                    self.pop_res();
                }
                VmOp::Halt => break,
            }
        }

        Ok(self.res_stack.value.clone())
    }

    /*** Evaluation Helpers ***/

    fn eval_symbol(&mut self, expr: ScmVal, name: &str) -> Result<(), ScmErr> {
        let value = match self.env.borrow().lookup(expr) {
            Some(val) => val,
            None => return Err(ScmErr::Undeclared(name.to_string())),
        };
        self.push_res(value);
        Ok(())
    }

    fn eval_pair(&mut self, cell: Rc<ConsCell>) -> Result<(), ScmErr> {
        match cell.head.clone() {
            ScmVal::Symbol(name) => self.eval_special(cell, &name.to_string())?,
            head => {
                let (args, dot, cycle) = ScmVal::list_to_vec(cell.tail.clone())
                    .ok_or(ScmErr::Syntax(ScmVal::Pair(Rc::clone(&cell))))?;
                if dot || cycle {
                    return Err(ScmErr::Syntax(ScmVal::Pair(Rc::clone(&cell))));
                }

                match head {
                    ScmVal::Core(_, n) => {
                        self.arity(args.len(), n as usize, &cell.head.to_string())?;
                        self.push_op(VmOp::Apply(cell.head.clone(), args.len()));
                        self.eval_many(args);
                    }
                    ScmVal::Pair(_) => {
                        self.push_op(VmOp::ApplyRes(args.len()));
                        self.push_op(VmOp::Eval(cell.head.clone()));
                        self.eval_many(args);
                    }
                    ScmVal::Closure(_) => {
                        self.push_op(VmOp::Apply(cell.head.clone(), args.len()));
                        self.eval_many(args);
                    }
                    _ => return Err(ScmErr::Syntax(ScmVal::Pair(cell.clone()))),
                }
            }
        }
        Ok(())
    }

    fn eval_special(&mut self, cell: Rc<ConsCell>, name: &str) -> Result<(), ScmErr> {
        let lookup = self.env.borrow().lookup(cell.head.clone());
        match lookup {
            Some(val) => self.push_op(VmOp::Eval(ScmVal::cons(val, cell.tail.clone()))),
            None => {
                let (args, dot, cycle) = ScmVal::list_to_vec(cell.tail.clone())
                    .ok_or(ScmErr::Syntax(ScmVal::Pair(Rc::clone(&cell))))?;
                if dot || cycle {
                    return Err(ScmErr::Syntax(ScmVal::Pair(Rc::clone(&cell))));
                }

                match name {
                    "if" => {
                        self.arity(args.len(), 2, "if")?;
                        self.push_op(VmOp::Apply(
                            ScmVal::Special(SpecialForm::If(args[1..].into())),
                            args.len() - 1,
                        ));
                        self.push_op(VmOp::Eval(args[0].clone()));
                    }
                    "lambda" => {
                        self.push_res(evh::make_closure(args, Rc::clone(&self.env))?);
                    }
                    "let" => {
                        self.push_op(VmOp::Eval(evh::transform_let(args, Rc::clone(&self.env))?));
                    }
                    "letrec" => {
                        self.push_op(VmOp::Eval(evh::transform_letrec(args)?));
                    }
                    "set!" => {
                        self.arity(args.len(), 2, "set!")?;
                        self.push_op(VmOp::Apply(
                            ScmVal::Special(SpecialForm::Set(Rc::new(args[0].clone()))),
                            args.len() - 1,
                        ));
                        self.push_op(VmOp::Eval(args[1].clone()));
                    }
                    "begin" => {
                        self.eval_body(args);
                    }
                    _ => return Err(ScmErr::Undeclared(name.to_string())),
                }
            }
        };
        Ok(())
    }

    fn eval_body(&mut self, args: Vec<ScmVal>) {
        self.push_op(VmOp::Eval(args[args.len() - 1].clone()));
        args[..args.len() - 1].iter().rev().for_each(|expr| {
            self.push_op(VmOp::Discard);
            self.push_op(VmOp::Eval(expr.clone()));
        })
    }

    fn eval_many(&mut self, args: Vec<ScmVal>) {
        // NOTE these get evaluated right to left, but end up
        // on the res_stack left to right once evaluated
        args.iter()
            .for_each(|v| self.push_op(VmOp::Eval(v.clone())));
    }

    /*** Application Helpers ***/

    fn apply(&mut self, val: ScmVal, num_args: usize) -> Result<(), ScmErr> {
        match val.clone() {
            ScmVal::Special(form) => self.apply_special(form)?,
            ScmVal::Core(b, _) => {
                let args = self.pop_n_res(num_args);
                self.push_res(proc::apply_core_proc(b, args)?);
            }
            ScmVal::Closure(c) => {
                self.push_op(VmOp::SetEnv(Rc::clone(&self.env)));
                self.env = evh::bind_closure_args(c.clone(), self.pop_n_res(num_args))?;
                self.eval_body(c.body.clone());
            }
            _ => return Err(ScmErr::Syntax(val)),
        }
        Ok(())
    }

    fn apply_special(&mut self, form: SpecialForm) -> Result<(), ScmErr> {
        match form {
            SpecialForm::If(args) => {
                let cond = self.pop_res();
                match proc::is_true(cond) {
                    true => self.push_op(VmOp::Eval(args[0].clone())),
                    false => match args.len() {
                        2.. => self.push_op(VmOp::Eval(args[1].clone())),
                        _ => self.push_res(ScmVal::Empty),
                    },
                }
            }
            SpecialForm::Set(key) => {
                let val = match self.pop_res() {
                    ScmVal::Closure(c) => ScmVal::new_closure(Closure::new(
                        &key.to_string(),
                        Rc::clone(&c.env),
                        c.params.clone(),
                        c.body.clone(),
                    )),
                    v => v,
                };
                self.env.borrow_mut().set((*key).clone(), val)?;
                self.push_res(ScmVal::Empty);
            }
        }
        Ok(())
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

    fn pop_n_res(&mut self, n: usize) -> Vec<ScmVal> {
        let mut args = Vec::new();
        for _ in 0..n {
            args.push(self.pop_res());
        }
        args
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
    use crate::types::Formals;

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

        let expr = StringReader::new("(if #t 1 0)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(1)));

        let expr = StringReader::new("(if #f 1 0)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(0)));

        let expr = StringReader::new("(if #t 1)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(1)));

        let expr = StringReader::new("(if #f 1)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Empty));
    }

    #[test]
    fn test_eval_builtin() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(cons 1 2)").read().unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(ScmVal::new_int(1), ScmVal::new_int(2)))
        );
    }

    #[test]
    fn test_eval_lambda() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(lambda (x) 1 2)").read().unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::Closure(Rc::new(Closure::new(
                "no-name",
                Rc::clone(&env),
                Formals::Fixed(vec![ScmVal::new_sym("x")]),
                vec![ScmVal::new_int(1), ScmVal::new_int(2)]
            ))))
        );
    }

    #[test]
    fn test_apply_lambda() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("((lambda (x) (cons x 2)) 1)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(ScmVal::new_int(1), ScmVal::new_int(2)))
        );

        let expr = StringReader::new("((lambda (x) 77 88 (cons x 2)) 1)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(ScmVal::new_int(1), ScmVal::new_int(2)))
        );

        let expr = StringReader::new("((lambda (x) (cons x 2)) 1 2 3 4)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(ScmVal::new_int(1), ScmVal::new_int(2)))
        );

        let expr = StringReader::new("((lambda x (cons 1 x)) 2 3 4)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(
                ScmVal::new_int(1),
                ScmVal::cons(
                    ScmVal::new_int(2),
                    ScmVal::cons(
                        ScmVal::new_int(3),
                        ScmVal::cons(ScmVal::new_int(4), ScmVal::Empty)
                    )
                )
            ))
        );
    }

    #[test]
    fn test_eval_let() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(let ((a 5)) a)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(let ((a 5) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(11)));

        let expr = StringReader::new("(let ((a (+ 3 4)) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(13)));

        let expr = StringReader::new("(let ((f (lambda (x) (+ x 1))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(7)));
    }

    #[test]
    fn test_eval_letrec() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(letrec ((a 5)) a)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(letrec ((f (lambda (x) (+ x 1))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(7)));

        let expr = StringReader::new(
            "(letrec ((f (lambda (x) (g x 1))) (g (lambda (x y) (- x y))) (b 10)) (f b))",
        )
        .read()
        .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(9)));
    }

    #[test]
    fn test_eval_begin() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(begin (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(12)));
    }
}
