use crate::builtin::Builtin;
use crate::core_proc as proc;
use crate::error::{ScmErr, ValResult};
use crate::eval_helpers as evh;
use crate::number::ScmNumber;
use crate::types::{Cell, Closure, Env, ScmVal, SpecialForm};
use std::rc::Rc;

// TODO A closure evaluation is not completely tail recursive. The environment
// switch is placed on the stack to allow the env to be replaced when we are done
// evaluating the body expressions. However, the environment is needed for the
// evaluation of a recursive call, so it is placed underneath the evaluation of
// the tail call. The closure for the function will have the env in it and will
// use it and once again put an env replace instruction on the stack. This will
// mean that we accumulate env replace instructions everytime that a recursive
// function is called. I think the most likely way to guard against this would
// be to check if the closure env is the one already on the stack. That way if
// we can avoid putting on another env every recursion. Another way would be to
// create another special form or special instruction that would pack the tail
// call with it's env and put them on the stack. Evaluating that would use the
// env stored in the instruction and both would be popped to be applied/evaluated.
// Then each subsequent recursion would replace it and only the one stack element
// with the env replace would be on the stack.
//
// TODO implement a continuation
// TODO a lot of the errors are just Syntax errors, which are not informative at
// all in many cases, especially if they do not hold the entire expression.
// TODO quasiquote, named let.
//
// TODO arity check closure applications to match the style of passed arguments.
// TODO arity is now checked before passing arguments to helpers and core procs,
// or it should be. Ensure all arity issues are checked by the vm before calling
// helpers and remove the arity checks from the helpers.
// TODO begin has two forms, one that is at the top level and is allowed to have
// defines inside it (I think). Currently, that will cause an error.
// TODO define is still allowed inside forms.
// TODO replace errors related to bindings with BadBinding
//
// TODO Macros -- note that I am not sure when macros should be expanded. It is
// relatively clear that they must be done at eval time since they are themselves
// evaluated, but perhaps top level expressions could be traversed once to expand
// any macros before evaluating the expression. This would keep macros from
// being present in a lambda body and having to be transformed every single call.
// Though perhaps this approach could be used when evaluating a lambda or any
// structure that might save code to be called later. Just macro expand the body
// when saving it. Then each call would use the full code. Probably macro expanding
// has to be recursive until there are no more macros to expand.

type OpStackRc = Rc<Stack<VmOp>>;
type ResStackRc = Rc<Stack<ScmVal>>;
type EnvRc = Rc<Env>;

// Virtual Machine ////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
enum VmOp {
    Eval(ScmVal),
    Apply(ScmVal, usize),
    UserApply(Vec<ScmVal>),
    UserEval(bool),
    //SetOpStack(EnvRc),
    //SetResStack(EnvRc),
    SetEnv(EnvRc),
    ApplyRes(usize),
    UnpackRes(String),
    Discard,
    Halt,
}

pub struct Vm {
    op_stack: OpStackRc,
    res_stack: ResStackRc,
    env: EnvRc,
}

impl Vm {
    pub fn new(env: Rc<Env>) -> Vm {
        Vm {
            op_stack: Stack::push(Stack::new_rc(VmOp::Halt, None), VmOp::Halt),
            res_stack: Stack::push(Stack::new_rc(ScmVal::Undefined, None), ScmVal::Undefined),
            env: env,
        }
    }

    pub fn eval_forms(&mut self, forms: &[ScmVal]) -> ValResult {
        if forms.len() < 1 {
            return Ok(ScmVal::Empty);
        }

        for f in forms[..forms.len() - 1].iter() {
            self.eval(f.clone())?;
        }
        self.eval(forms[forms.len() - 1].clone())
    }

    pub fn eval(&mut self, form: ScmVal) -> ValResult {
        self.init_stacks();
        self.op_stack = Stack::push(Rc::clone(&self.op_stack), VmOp::Eval(form));

        loop {
            let op = self.pop_op();
            match op {
                VmOp::Eval(ref expr) => match expr {
                    ScmVal::NewSymbol(name) => self.eval_symbol(expr, &name.to_string())?,
                    ScmVal::NewPair(cell) => {
                        self.eval_pair(Rc::clone(cell))?;
                    }
                    ScmVal::Number(_)
                    | ScmVal::Boolean(_)
                    | ScmVal::Character(_)
                    | ScmVal::Closure(_)
                    | ScmVal::Core(_, _)
                    | ScmVal::Env(_)
                    | ScmVal::NewString(_)
                    | ScmVal::Undefined
                    | ScmVal::Empty => self.push_res(expr.clone()),
                    _ => return Err(ScmErr::Syntax(expr.clone())),
                },
                VmOp::Apply(val, n) => {
                    self.apply(val, n)?;
                }
                VmOp::UserApply(args) => {
                    let size = match self.pop_res() {
                        ScmVal::Number(ScmNumber::Integer(i)) => i as usize,
                        _ => panic!("apply should have put an integer on top of res stack"),
                    };
                    self.push_op(VmOp::ApplyRes((args.len() - 1) + size));
                    self.eval_many(&args);
                }
                VmOp::UserEval(has_env) => {
                    if has_env {
                        let env = match self.pop_res() {
                            ScmVal::Env(e) => e,
                            val => {
                                return Err(ScmErr::BadArgType(
                                    "eval".to_owned(),
                                    "environment".to_owned(),
                                    val.clone(),
                                ))
                            }
                        };
                        self.push_op(VmOp::SetEnv(Rc::clone(&self.env)));
                        self.env = env;
                    }
                    let expr = self.pop_res();
                    self.push_op(VmOp::Eval(expr));
                }
                //VmOp::SetOpStack(val) => {}
                //VmOp::SetResStack(val) => {}
                VmOp::SetEnv(env) => {
                    self.env = env;
                }
                VmOp::ApplyRes(n) => {
                    let proc = self.pop_res();
                    self.push_op(VmOp::Apply(proc, n));
                }
                VmOp::UnpackRes(name) => {
                    let list = self.pop_res();
                    let (args, dot) = ScmVal::list_to_vec(&list).ok_or(ScmErr::BadArgType(
                        name.clone(),
                        "list".to_owned(),
                        list.clone(),
                    ))?;
                    if dot {
                        return Err(ScmErr::BadArgType(
                            name.clone(),
                            "list".to_owned(),
                            list.clone(),
                        ));
                    }
                    let length = args.len();
                    args.into_iter().rev().for_each(|a| self.push_res(a));
                    self.push_res(ScmVal::new_int(length as i64));
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

    fn eval_symbol(&mut self, expr: &ScmVal, name: &str) -> Result<(), ScmErr> {
        let value = match self.env.lookup(expr) {
            Some(val) => val,
            None => return Err(ScmErr::Undeclared(name.to_string())),
        };
        self.push_res(value);
        Ok(())
    }

    // TODO no longer checks for cyclic
    fn eval_pair(&mut self, cell: Rc<Cell>) -> Result<(), ScmErr> {
        match cell.clone_head() {
            ScmVal::NewSymbol(name) => {
                self.eval_special(cell, &name.to_string())?;
            }
            head => {
                let (args, dot) = ScmVal::list_to_vec(&cell.clone_tail())
                    .ok_or(ScmErr::Syntax(ScmVal::NewPair(Rc::clone(&cell))))?;
                if dot {
                    return Err(ScmErr::Syntax(ScmVal::NewPair(Rc::clone(&cell))));
                }

                match head {
                    ScmVal::Core(Builtin::Apply, n) => {
                        self.arity(args.len(), n as usize, "apply")?;
                        self.push_op(VmOp::UserApply(args[..args.len() - 1].into()));
                        self.push_op(VmOp::UnpackRes("apply".to_owned()));
                        self.push_op(VmOp::Eval(args[args.len() - 1].clone()));
                    }
                    ScmVal::Core(Builtin::Eval, n) => {
                        self.arity(args.len(), n as usize, "eval")?;
                        let has_env = args.len() > 1;
                        self.push_op(VmOp::UserEval(has_env));
                        if has_env {
                            self.push_op(VmOp::Eval(args[1].clone()));
                        }
                        self.push_op(VmOp::Eval(args[0].clone()));
                    }
                    ScmVal::Core(b, n) => {
                        self.arity(args.len(), n as usize, &b.to_string())?;
                        self.push_op(VmOp::Apply(cell.clone_head(), args.len()));
                        self.eval_many(&args);
                    }
                    ScmVal::NewPair(_) => {
                        self.push_op(VmOp::ApplyRes(args.len()));
                        self.push_op(VmOp::Eval(cell.clone_head()));
                        self.eval_many(&args);
                    }
                    ScmVal::Closure(_) => {
                        self.push_op(VmOp::Apply(cell.clone_head(), args.len()));
                        self.eval_many(&args);
                    }
                    _ => return Err(ScmErr::Syntax(ScmVal::NewPair(cell.clone()))),
                }
            }
        }
        Ok(())
    }

    // TODO no longer checks for cyclic
    fn eval_special(&mut self, cell: Rc<Cell>, name: &str) -> Result<(), ScmErr> {
        let lookup = self.env.lookup(&cell.clone_head());
        match lookup {
            Some(val) => {
                self.push_op(VmOp::Eval(ScmVal::cons(val.clone(), cell.clone_tail())));
            }
            None => {
                let (mut args, dot) = ScmVal::list_to_vec(&cell.clone_tail())
                    .ok_or(ScmErr::Syntax(ScmVal::NewPair(Rc::clone(&cell))))?;
                if dot {
                    return Err(ScmErr::Syntax(ScmVal::NewPair(Rc::clone(&cell))));
                }
                let num_args = args.len();

                match name {
                    "quote" => {
                        self.arity(args.len(), 1, "quote")?;
                        self.push_res(args[0].clone());
                    }
                    "define" => self.eval_define(&args)?,
                    "if" => {
                        self.arity(args.len(), 2, "if")?;
                        self.push_op(VmOp::Apply(
                            ScmVal::new_if(args[1..].into()),
                            args.len() - 1,
                        ));
                        self.push_op(VmOp::Eval(args[0].clone()));
                    }
                    "lambda" => {
                        self.arity(num_args, 2, "lambda")?;
                        self.push_res(evh::make_closure(&args, Rc::clone(&self.env))?);
                    }
                    "let" => {
                        self.arity(num_args, 2, "let")?;
                        self.push_op(VmOp::Eval(evh::transform_let(&args, Rc::clone(&self.env))?));
                    }
                    "let*" => {
                        self.arity(num_args, 2, "let*")?;
                        self.push_op(VmOp::Eval(evh::transform_let_star(&args)?));
                    }
                    "letrec" => {
                        self.arity(num_args, 2, "letrec")?;
                        self.push_op(VmOp::Eval(evh::transform_letrec(&args)?));
                    }
                    "set!" => {
                        self.arity(args.len(), 2, "set!")?;
                        self.push_op(VmOp::Apply(ScmVal::new_set(args[0].clone()), 1));
                        self.push_op(VmOp::Eval(args[1].clone()));
                    }
                    "begin" => self.eval_body(&args),
                    "do" => {
                        self.arity(num_args, 2, "do")?;
                        self.push_op(VmOp::Eval(evh::transform_do(&args)?));
                    }

                    // These are not in separate methods because they tread args and mutable
                    "and" => match num_args {
                        // NOTE this is almost the same as the apply version, but
                        // we should only ever see the expr in eval once where we
                        // have to do a little extra work.
                        2.. => {
                            let first = args[0].clone();
                            args.reverse();
                            args.pop();
                            self.push_op(VmOp::Apply(ScmVal::new_and(args), 1));
                            self.push_op(VmOp::Eval(first));
                        }
                        1 => self.push_op(VmOp::Eval(args[0].clone())),
                        _ => self.push_res(ScmVal::Boolean(true)),
                    },
                    "or" => match num_args {
                        2.. => {
                            let first = args[0].clone();
                            args.reverse();
                            args.pop();
                            self.push_op(VmOp::Apply(ScmVal::new_or(args), 1));
                            self.push_op(VmOp::Eval(first));
                        }
                        1 => self.push_op(VmOp::Eval(args[0].clone())),
                        _ => self.push_res(ScmVal::Boolean(false)),
                    },
                    "cond" => {
                        self.arity(num_args, 1, "cond")?;
                        let (test, arrow, branch) = self.separate_cond_clause(args[0].clone())?;
                        if test == ScmVal::new_sym("else") {
                            self.eval_body(&branch);
                        } else {
                            args.reverse();
                            args.pop();
                            self.push_op(VmOp::Apply(ScmVal::new_cond(branch, arrow, args), 1));
                            self.push_op(VmOp::Eval(test));
                        }
                    }
                    "case" => {
                        self.arity(num_args, 2, "case")?;
                        let first = args[0].clone();
                        args.reverse();
                        args.pop();
                        self.push_op(VmOp::Apply(ScmVal::new_case(ScmVal::Undefined, args), 1));
                        self.push_op(VmOp::Eval(first));
                    }
                    _ => return Err(ScmErr::Undeclared(name.to_string())),
                }
            }
        };
        Ok(())
    }

    fn eval_body(&mut self, args: &[ScmVal]) {
        self.push_op(VmOp::Eval(args[args.len() - 1].clone()));
        args[..args.len() - 1].iter().rev().for_each(|expr| {
            self.push_op(VmOp::Discard);
            self.push_op(VmOp::Eval(expr.clone()));
        });
    }

    fn eval_many(&mut self, args: &[ScmVal]) {
        // NOTE these get evaluated right to left, but end up
        // on the res_stack left to right once evaluated
        args.iter()
            .for_each(|v| self.push_op(VmOp::Eval(v.clone())));
    }

    // TODO add something to ensure it won't happen if not at top level
    fn eval_define(&mut self, args: &[ScmVal]) -> Result<(), ScmErr> {
        let num_args = args.len();
        self.arity(num_args, 2, "define")?;

        let first = args[0].clone();
        match first {
            ScmVal::NewSymbol(_) => {
                self.env.insert(first.clone(), ScmVal::Undefined)?;
                self.push_op(VmOp::Apply(ScmVal::new_set(first), num_args - 1));
                self.push_op(VmOp::Eval(args[1].clone()));
            }
            ScmVal::NewPair(cell) => {
                self.define_to_lambda(&args[1..], cell.clone_head(), cell.clone_tail());
            }
            _ => {
                return Err(ScmErr::BadArgType(
                    "define".to_owned(),
                    "pair or symbol".to_owned(),
                    first,
                ))
            }
        };
        Ok(())
    }

    fn define_to_lambda(&mut self, args: &[ScmVal], head: ScmVal, tail: ScmVal) {
        let name = head;
        let params = tail;
        let body = ScmVal::vec_to_list(&args, ScmVal::Empty);
        let lambda = ScmVal::cons(ScmVal::new_sym("lambda"), ScmVal::cons(params, body));
        self.push_op(VmOp::Eval(ScmVal::vec_to_list(
            &[ScmVal::new_sym("define"), name, lambda],
            ScmVal::Empty,
        )));
    }

    /*** Application Helpers ***/

    fn apply(&mut self, val: ScmVal, num_args: usize) -> Result<(), ScmErr> {
        match val.clone() {
            ScmVal::Special(form) => self.apply_special(form)?,
            ScmVal::Core(b, _) => {
                let args = self.pop_n_res(num_args);
                self.push_res(proc::apply_core_proc(b, &args)?);
            }
            ScmVal::Closure(c) => {
                // arity is checked during binding based on the param types
                self.push_op(VmOp::SetEnv(Rc::clone(&self.env)));
                self.env = evh::bind_closure_args(c.clone(), &self.pop_n_res(num_args))?;
                self.eval_body(&c.body);
            }
            _ => return Err(ScmErr::Syntax(val)),
        }
        Ok(())
    }

    fn apply_special(&mut self, form: Box<SpecialForm>) -> Result<(), ScmErr> {
        match *form {
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
                self.env.set(key.clone(), val)?;
                self.push_res(ScmVal::Empty);
            }
            SpecialForm::And(mut args) => {
                let cond = self.pop_res();
                let num_args = args.len();
                if proc::is_true(cond.clone()) {
                    match num_args {
                        2.. => {
                            // args is reversed so we want the last element
                            let first = args[num_args - 1].clone();
                            args.pop();
                            self.push_op(VmOp::Apply(ScmVal::new_and(args), 1));
                            self.push_op(VmOp::Eval(first));
                        }
                        1 => self.push_op(VmOp::Eval(args[0].clone())),
                        _ => panic!("cannot apply and with 0 arguments"),
                    }
                } else {
                    self.push_res(cond);
                }
            }
            SpecialForm::Or(mut args) => {
                let cond = self.pop_res();
                let num_args = args.len();
                if !proc::is_true(cond.clone()) {
                    match num_args {
                        2.. => {
                            let first = args[num_args - 1].clone();
                            args.pop();
                            self.push_op(VmOp::Apply(ScmVal::new_or(args), 1));
                            self.push_op(VmOp::Eval(first));
                        }
                        1 => self.push_op(VmOp::Eval(args[0].clone())),
                        _ => self.push_res(ScmVal::Boolean(false)),
                    }
                } else {
                    self.push_res(cond);
                }
            }
            SpecialForm::Cond(body, arrow, mut args) => {
                let cond = self.pop_res();
                let num_args = args.len();
                if proc::is_true(cond.clone()) {
                    if arrow {
                        self.push_res(cond);
                        self.push_op(VmOp::ApplyRes(1));
                        self.push_op(VmOp::Eval(body[0].clone()));
                    } else {
                        self.eval_body(&body);
                    }
                } else if num_args >= 1 {
                    let (test, arrow, branch) =
                        self.separate_cond_clause(args[num_args - 1].clone())?;
                    if test == ScmVal::new_sym("else") {
                        self.eval_body(&branch);
                    } else {
                        args.pop();
                        self.push_op(VmOp::Apply(ScmVal::new_cond(branch, arrow, args), 1));
                        self.push_op(VmOp::Eval(test));
                    }
                } else {
                    self.push_res(ScmVal::Empty);
                }
            }
            SpecialForm::Case(val, mut args) => match val {
                ScmVal::Undefined => {
                    let case = self.pop_res();
                    self.push_op(VmOp::Apply(ScmVal::new_case(case, args), 1));
                }
                _ => {
                    let (cases, is_else, branch) =
                        self.separate_case_clause(args[args.len() - 1].clone())?;
                    if is_else || cases.iter().any(|c| *c == val) {
                        self.eval_body(&branch);
                    } else {
                        args.pop();
                        self.push_op(VmOp::Apply(ScmVal::new_case(val, args), 1));
                    }
                }
            },
        }
        Ok(())
    }

    fn separate_cond_clause(&self, expr: ScmVal) -> Result<(ScmVal, bool, Vec<ScmVal>), ScmErr> {
        let (args, dot) = ScmVal::list_to_vec(&expr).ok_or(ScmErr::Syntax(expr.clone()))?;
        if dot {
            return Err(ScmErr::Syntax(expr));
        }
        let num_args = args.len();
        match num_args {
            3.. => {
                // TODO can these be passed back as slices?
                if args[1] == ScmVal::new_sym("=>") {
                    Ok((args[0].clone(), true, args[2..3].into()))
                } else {
                    Ok((args[0].clone(), false, args[1..].into()))
                }
            }
            2 => Ok((args[0].clone(), false, args[1..].into())),
            _ => Err(ScmErr::Syntax(expr)),
        }
    }

    fn separate_case_clause(
        &self,
        expr: ScmVal,
    ) -> Result<(Vec<ScmVal>, bool, Vec<ScmVal>), ScmErr> {
        let (args, dot) = ScmVal::list_to_vec(&expr).ok_or(ScmErr::Syntax(expr.clone()))?;
        if dot {
            return Err(ScmErr::Syntax(expr));
        }
        let num_args = args.len();
        match num_args {
            2.. => {
                // TODO can these be passed back as slices?
                if args[0].clone() == ScmVal::new_sym("else") {
                    Ok((vec![], true, args[1..].into()))
                } else {
                    let (cases, dot) =
                        ScmVal::list_to_vec(&args[0]).ok_or(ScmErr::Syntax(expr.clone()))?;
                    if dot {
                        return Err(ScmErr::Syntax(expr));
                    }
                    Ok((cases, false, args[1..].into()))
                }
            }
            _ => Err(ScmErr::Syntax(expr)),
        }
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
        let env =
            Rc::new(Env::new_with_bindings(&[(ScmVal::new_sym("A"), ScmVal::new_int(5))]).unwrap());
        let mut vm = Vm::new(Rc::clone(&env));
        assert_eq!(vm.eval(ScmVal::new_sym("A")), Ok(ScmVal::new_int(5)));
        assert_eq!(
            vm.eval(ScmVal::new_sym("B")),
            Err(ScmErr::Undeclared("B".to_owned()))
        );
    }

    #[test]
    fn test_eval_simple_if() {
        let env = Rc::new(Env::new());
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
    fn test_eval_quote() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(cons 1 '(2))").read().unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::cons(
                ScmVal::new_int(1),
                ScmVal::new_pair(ScmVal::new_int(2), ScmVal::Empty)
            ))
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

        let expr = StringReader::new("((lambda () 2))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(2)));

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
    fn test_eval_let_star() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(let* () 1 2 3 4 5)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(let* ((a 5)) a)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(let* ((a 5) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(11)));

        let expr = StringReader::new("(let* ((a (+ 3 4)) (b a)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(14)));

        let expr = StringReader::new("(let* ((a 2) (f (lambda (x) (+ x a))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(8)));
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

    #[test]
    fn test_eval_and() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(and (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(12)));

        let expr = StringReader::new("(and (+ 1 2) (- 1 2) #f (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Boolean(false)));

        let expr = StringReader::new("(and)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Boolean(true)));

        let expr = StringReader::new("(let ((a 10)) (and (set! a 5) a))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));
    }

    #[test]
    fn test_eval_or() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(or (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(3)));

        let expr = StringReader::new("(or #f (= 1 3) (< 99 1))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Boolean(false)));

        let expr = StringReader::new("(or #f (* 3 4))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(12)));

        let expr = StringReader::new("(or)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Boolean(false)));

        let expr = StringReader::new("(let ((a 10)) (or (set! a 5) (set! a 20)) a)")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));
    }

    #[test]
    fn test_eval_do() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(do ((i 0 (+ i 1))) ((= i 5) (+ i 5)))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(10)));

        let expr = StringReader::new(
            "(do ((vec (make-vector 5))
                  (i 0 (+ i 1)))
                 ((eqv? i 5) vec)
               (vector-set! vec i i))",
        )
        .read()
        .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(ScmVal::new_vec_mut(vec![
                ScmVal::new_int(0),
                ScmVal::new_int(1),
                ScmVal::new_int(2),
                ScmVal::new_int(3),
                ScmVal::new_int(4),
            ]))
        );
    }

    #[test]
    fn eval_multiple_forms() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let forms = StringReader::new("(+ 1 2) (+ 5 6)").read_forms().unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(ScmVal::new_int(11)));
    }

    #[test]
    fn test_eval_define() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let forms = StringReader::new("(define (f) 13) (f)")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(ScmVal::new_int(13)));

        let forms = StringReader::new("(define a 5)").read_forms().unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(ScmVal::Empty));

        let forms = StringReader::new("(define a 5) (+ a 10)")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(ScmVal::new_int(15)));

        let forms = StringReader::new(
            "(define vec (make-vector 5))
             (do ((i 0 (+ i 1)))
                 ((eqv? i 5) vec)
               (vector-set! vec i i))",
        )
        .read_forms()
        .unwrap();
        assert_eq!(
            vm.eval_forms(&forms),
            Ok(ScmVal::new_vec_mut(vec![
                ScmVal::new_int(0),
                ScmVal::new_int(1),
                ScmVal::new_int(2),
                ScmVal::new_int(3),
                ScmVal::new_int(4),
            ]))
        );

        let forms = StringReader::new("(define (f . x) (cons 1 x)) (f 2)")
            .read_forms()
            .unwrap();
        assert_eq!(
            vm.eval_forms(&forms),
            Ok(ScmVal::cons(
                ScmVal::new_int(1),
                ScmVal::cons(ScmVal::new_int(2), ScmVal::Empty)
            ))
        );
    }

    #[test]
    fn test_eval_cond() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(cond (#t 7) (#t 9) (#t 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(7)));

        let expr = StringReader::new("(cond (#f 7) (#t 9) (#t 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(9)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#t 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#t 1 2 3 4 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(5)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#f 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::Empty));

        let expr = StringReader::new("(cond (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(33)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(33)));

        let expr = StringReader::new("(cond ((+ 1 2) => -) (#f 9) (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(-3)));
    }

    #[test]
    fn test_eval_case() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(case (+ 2 1) ((1 2 3) 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_sym("a")));

        let expr = StringReader::new("(case (+ 2 3) ((1 2 3) 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_sym("b")));

        let expr = StringReader::new("(case (+ 2 3) ((1 2 3) 'a) ((a b c) 'b) (else 1 2 3 4 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_sym("c")));

        let expr = StringReader::new("(case (+ 2 1) (() 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_sym("c")));
    }

    #[test]
    fn test_eval_user_apply() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(apply + '(1 2 3 4))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(10)));

        let expr = StringReader::new("(apply + 1 2 3 '(5))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(11)));

        let expr = StringReader::new("(apply + 1 2 (+ 1 2) (cons 6 '())))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(12)));

        let expr = StringReader::new(
            "(let ((compose (lambda (f g)
                              (lambda args
                                (f (apply g args))))))
               ((compose - +) 1 2 3 4))",
        )
        .read()
        .unwrap();
        assert_eq!(vm.eval(expr), Ok(ScmVal::new_int(-10)));
    }

    #[test]
    fn test_eval_user_eval() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(define + -) (eval '(+ 1 2) (null-environment))")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&expr), Ok(ScmVal::new_int(3)));

        let expr = StringReader::new("(define + -) (eval '(+ 1 2))")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&expr), Ok(ScmVal::new_int(-1)));
    }
}
