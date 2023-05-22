use crate::cell::Cell;
use crate::env::Env;
use crate::err::{Error, UserError};
use crate::eval_helpers as evh;
use crate::other_procs::are_eqv;
use crate::proc::{Closure, Proc};
use crate::proc_utils as utils;
use crate::string::Str;
use crate::types::Type;
use crate::value::{SpecialForm, Value};
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
type ResStackRc = Rc<Stack<Value>>;
type EnvRc = Rc<Env<Str, Value>>;

// Virtual Machine ////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
enum VmOp {
    Eval(Value),
    Apply(Value),
    UserEval(bool),
    //SetOpStack(EnvRc),
    //SetResStack(EnvRc),
    SetEnv(EnvRc),
    PackRes(usize),
    ApplyRes,
    Discard,
    Halt,
}

pub struct Vm {
    op_stack: OpStackRc,
    res_stack: ResStackRc,
    env: EnvRc,
}

impl Vm {
    /*** Constructor ***/

    pub fn new(env: Rc<Env<Str, Value>>) -> Vm {
        Vm {
            op_stack: Stack::push(Stack::new_rc(VmOp::Halt, None), VmOp::Halt),
            res_stack: Stack::push(Stack::new_rc(Value::Undefined, None), Value::Undefined),
            env: env,
        }
    }

    /*** Evaluate Forms ***/

    pub fn eval_forms(&mut self, forms: &[Value]) -> Result<Value, UserError> {
        if forms.len() < 1 {
            return Ok(Value::Empty);
        }

        for f in forms[..forms.len() - 1].iter() {
            self.eval(f.clone())?;
        }
        self.eval(forms[forms.len() - 1].clone())
    }

    pub fn eval(&mut self, form: Value) -> Result<Value, UserError> {
        self.init_stacks();
        self.op_stack = Stack::push(Rc::clone(&self.op_stack), VmOp::Eval(form));

        loop {
            let op = self.pop_op();
            match op {
                VmOp::Eval(ref expr) => match expr {
                    Value::Symbol(name) => self.eval_symbol(name)?,
                    Value::Pair(cell) => {
                        self.eval_pair(Rc::clone(cell))?;
                    }
                    Value::Number(_)
                    | Value::Bool(_)
                    | Value::Char(_)
                    | Value::Closure(_)
                    | Value::Procedure(_)
                    | Value::Env(_)
                    | Value::String(_)
                    | Value::Undefined
                    | Value::Empty => self.push_res(expr.clone()),
                    _ => return Err(UserError::Syntax(expr.clone())),
                },
                VmOp::Apply(val) => {
                    self.apply(val)?;
                }
                VmOp::UserEval(has_env) => {
                    if has_env {
                        let env = match self.pop_res() {
                            Value::Env(e) => e,
                            val => return Err(UserError::bad_arg("eval", Type::Env, val.clone())),
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
                VmOp::PackRes(n) => {
                    let mut result = Value::Empty;
                    for _ in 0..n {
                        result = Value::from(Cell::new(self.pop_res(), Some(result)));
                    }
                    self.push_res(result);
                }
                VmOp::ApplyRes => {
                    let proc = self.pop_res();
                    self.push_op(VmOp::Apply(proc));
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

    fn eval_symbol(&mut self, string: &Str) -> Result<(), UserError> {
        let value = match self.env.lookup(string) {
            Some(val) => val,
            None => return Err(UserError::Undeclared(string.clone())),
        };
        self.push_res(value);
        Ok(())
    }

    fn eval_pair(&mut self, cell: Rc<Cell<Value>>) -> Result<(), UserError> {
        let args = match cell.tail().clone() {
            Some(val) => val,
            None => Value::Empty,
        };

        match cell.head().clone() {
            Value::Symbol(name) => {
                self.eval_special(&name, &args)?;
            }
            head => match head {
                Value::Procedure(proc) => self.eval_proc(proc, &args)?,
                Value::Pair(_) => {
                    self.push_op(VmOp::ApplyRes);
                    self.push_op(VmOp::Eval(head));
                    self.eval_list(&args);
                }
                Value::Closure(_) => {
                    self.push_op(VmOp::Apply(head));
                    self.eval_list(&args);
                }
                _ => return Err(UserError::Syntax(Value::Pair(cell.clone()))),
            },
        }
        Ok(())
    }

    fn eval_proc(&mut self, proc: Rc<Proc<Value>>, args: &Value) -> Result<(), UserError> {
        let proc_val = Value::Procedure(proc.clone());
        match proc.name.to_string().as_str() {
            "apply" => {
                let (func, rest) = utils::rest_take_1(args).or(Err(UserError::Arity(proc_val)))?;
                let cell = Value::get_pair_cell(&rest).ok_or(UserError::Syntax(args.clone()))?;
                let vec: Vec<Value> = cell.values().collect();
                let last = vec[vec.len() - 1].clone();
                let args = match Value::get_pair_cell(&last) {
                    Some(_) => {
                        let mut result = last;
                        for val in vec.iter() {
                            result = Value::from(Cell::new(val.clone(), Some(result)));
                        }
                        result
                    }
                    None => return Err(UserError::bad_arg("apply", Type::Pair, last.clone())),
                };
                self.push_op(VmOp::Apply(func));
                self.eval_list(&args);
            }
            "eval" => {
                let (expr, opt) =
                    utils::opt_last_take_2(args).or(Err(UserError::Arity(proc_val)))?;
                match opt {
                    Some(env_expr) => {
                        self.push_op(VmOp::UserEval(true));
                        self.push_op(VmOp::Eval(env_expr.clone()));
                    }
                    None => {
                        self.push_op(VmOp::UserEval(false));
                    }
                }
                self.push_op(VmOp::Eval(expr.clone()));
            }
            _ => {
                self.push_op(VmOp::Apply(proc_val));
                self.eval_list(&args);
            }
        }
        Ok(())
    }

    fn eval_special(&mut self, name: &Str, args: &Value) -> Result<(), UserError> {
        let lookup = self.env.lookup(name);
        match lookup {
            Some(val) => {
                self.push_op(VmOp::Eval(Value::from(Cell::new(
                    val.clone(),
                    Some(args.clone()),
                ))));
            }
            None => {
                let expr = Value::from(Cell::new(Value::symbol(name.clone()), Some(args.clone())));
                match name.to_string().as_str() {
                    "quote" => {
                        let arg = utils::fixed_take_1(args).or(Err(UserError::Syntax(expr)))?;
                        self.push_res(arg);
                    }
                    "define" => self.eval_define(args)?,
                    "if" => {
                        let (cond, t, f) =
                            utils::opt_last_take_3(args).or(Err(UserError::Syntax(expr)))?;
                        self.push_op(VmOp::Apply(Value::from(SpecialForm::If(t, f))));
                        self.push_op(VmOp::Eval(cond));
                    }
                    "lambda" => self.push_res(
                        evh::make_closure(args, Rc::clone(&self.env))
                            .or(Err(UserError::Syntax(expr)))?,
                    ),
                    "let" => {
                        self.push_op(VmOp::Eval(
                            evh::transform_let(args, Rc::clone(&self.env))
                                .or(Err(UserError::Syntax(expr)))?,
                        ));
                    }
                    "let*" => {
                        self.push_op(VmOp::Eval(
                            evh::transform_let_star(args).or(Err(UserError::Syntax(expr)))?,
                        ));
                    }
                    "letrec" => {
                        self.push_op(VmOp::Eval(
                            evh::transform_letrec(args).or(Err(UserError::Syntax(expr)))?,
                        ));
                    }
                    "set!" => {
                        let arg = utils::fixed_take_1(args).or(Err(UserError::Syntax(expr)))?;
                        self.push_op(VmOp::Apply(Value::from(SpecialForm::Set(name.clone()))));
                        self.push_op(VmOp::Eval(arg));
                    }
                    "begin" => self.eval_body(args),
                    "do" => {
                        self.push_op(VmOp::Eval(
                            evh::transform_do(&args).or(Err(UserError::Syntax(expr)))?,
                        ));
                    }

                    // TODO make separate methods
                    "and" => {
                        if let Value::Empty = args {
                            self.push_res(Value::Bool(true));
                        } else {
                            let (first, rest) =
                                utils::rest_take_1(args).or(Err(UserError::Syntax(expr)))?;
                            match rest {
                                Value::Empty => self.push_op(VmOp::Eval(first)),
                                _ => {
                                    self.push_op(VmOp::Apply(Value::from(SpecialForm::And(rest))));
                                    self.push_op(VmOp::Eval(first));
                                }
                            }
                        }
                    }
                    "or" => {
                        if let Value::Empty = args {
                            self.push_res(Value::Bool(false));
                        } else {
                            let (first, rest) =
                                utils::rest_take_1(args).or(Err(UserError::Syntax(expr)))?;
                            match rest {
                                Value::Empty => self.push_op(VmOp::Eval(first)),
                                _ => {
                                    self.push_op(VmOp::Apply(Value::from(SpecialForm::Or(rest))));
                                    self.push_op(VmOp::Eval(first));
                                }
                            }
                        }
                    }
                    "cond" => {
                        let (first, rest) =
                            utils::rest_take_1(args).or(Err(UserError::Syntax(expr)))?;
                        let (test, arrow, branch) = self.separate_cond_clause(first)?;
                        if test == Value::from(Str::from("else")) {
                            self.eval_body(&branch);
                        } else {
                            self.push_op(VmOp::Apply(Value::from(SpecialForm::Cond(
                                branch, arrow, rest,
                            ))));
                            self.push_op(VmOp::Eval(test));
                        }
                    }
                    "case" => {
                        let (first, rest) =
                            utils::rest_take_1(args).or(Err(UserError::Syntax(expr)))?;
                        self.push_op(VmOp::Apply(Value::from(SpecialForm::Case(
                            Value::Undefined,
                            rest,
                        ))));
                        self.push_op(VmOp::Eval(first));
                    }
                    _ => return Err(UserError::Undeclared(name.clone())),
                }
            }
        };
        Ok(())
    }

    fn eval_body(&mut self, args: &Value) {
        let vec: Vec<Value> = match Value::get_pair_cell(args) {
            Some(cell) => cell.values().collect(),
            None => return self.push_op(VmOp::Eval(Value::Empty)),
        };

        self.push_op(VmOp::Eval(vec[vec.len() - 1].clone()));
        vec[..vec.len() - 1].iter().rev().for_each(|expr| {
            self.push_op(VmOp::Discard);
            self.push_op(VmOp::Eval(expr.clone()));
        });
    }

    fn eval_list(&mut self, args: &Value) {
        let vec: Vec<Value> = match Value::get_pair_cell(args) {
            Some(cell) => cell.values().collect(),
            None => return self.push_op(VmOp::Eval(Value::Empty)),
        };

        self.push_op(VmOp::PackRes(vec.len()));
        vec.iter()
            .rev()
            .for_each(|v| self.push_op(VmOp::Eval(v.clone())))
    }

    // TODO add something to ensure it won't happen if not at top level
    fn eval_define(&mut self, args: &Value) -> Result<(), UserError> {
        let expr = Value::from(Cell::new(
            Value::from(Str::from("define")),
            Some(args.clone()),
        ));

        let (first, rest) = utils::rest_take_1(args).or(Err(UserError::Syntax(expr.clone())))?;
        match first {
            Value::Symbol(name) => {
                let second = utils::fixed_take_1(&rest).or(Err(UserError::Syntax(expr)))?;
                self.env.insert((*name).clone(), Value::Undefined);
                self.push_op(VmOp::Apply(Value::from(SpecialForm::Set((*name).clone()))));
                self.push_op(VmOp::Eval(second));
            }
            Value::Pair(cell) => {
                self.define_to_lambda(rest, cell);
            }
            _ => {
                return Err(UserError::Syntax(expr));
            }
        };
        Ok(())
    }

    fn define_to_lambda(&mut self, body: Value, cell: Rc<Cell<Value>>) {
        let name = cell.head().clone();
        let params = match cell.tail().clone() {
            Some(val) => val,
            None => Value::Empty,
        };
        let lambda = Value::list_from_vec(vec![Value::from(Str::from("lambda")), params], body);
        self.push_op(VmOp::Eval(Value::list_from_vec(
            vec![Value::from(Str::from("define")), name, lambda],
            Value::Empty,
        )));
    }

    /*** Application Helpers ***/

    fn apply(&mut self, val: Value) -> Result<(), UserError> {
        match val.clone() {
            Value::Special(form) => self.apply_special(form)?,
            Value::Procedure(p) => {
                let args = self.pop_res();
                let func = p.func;
                let result = match func(&args) {
                    Ok(v) => v,
                    Err(Error::BadArg(i)) => {
                        return Err(UserError::bad_arg(&p.name.to_string(), p.arity.get(i), val))
                    }
                    Err(Error::Arity) => {
                        return Err(UserError::Arity(val));
                    }
                    Err(Error::ArgsNotList) => {
                        panic!("arguments passed to procedures should be pair");
                    }
                    Err(_) => return Err(UserError::Syntax(val)),
                };
                self.push_res(result);
            }
            Value::Closure(c) => {
                // arity is checked during binding based on the param types
                self.push_op(VmOp::SetEnv(Rc::clone(&self.env)));
                self.env = evh::bind_closure_args(c.clone(), &self.pop_res())
                    .or(Err(UserError::Syntax(val)))?;
                self.eval_body(&c.body);
            }
            _ => return Err(UserError::Syntax(val)),
        }
        Ok(())
    }

    fn apply_special(&mut self, form: Box<SpecialForm>) -> Result<(), UserError> {
        match *form {
            SpecialForm::If(t, f) => {
                let cond = self.pop_res();
                match cond.is_true() {
                    true => self.push_op(VmOp::Eval(t)),
                    false => match f {
                        Some(expr) => self.push_op(VmOp::Eval(expr)),
                        None => self.push_res(Value::Empty),
                    },
                }
            }
            SpecialForm::Set(key) => {
                let val = match self.pop_res() {
                    Value::Closure(c) => Value::from(Closure::new(
                        Some(key.clone()),
                        Rc::clone(&c.env),
                        c.formals.clone(),
                        c.body.clone(),
                    )),
                    v => v,
                };
                if !self.env.set(key.clone(), val) {
                    return Err(UserError::Undeclared(key));
                }
                self.push_res(Value::Empty);
            }
            SpecialForm::And(args) => {
                let cond = self.pop_res();
                if cond.is_true() {
                    if let Value::Empty = args {
                        self.push_res(Value::Bool(true));
                    } else {
                        let (first, rest) = utils::rest_take_1(&args)
                            .expect("should have at least one val if not Empty");
                        match rest {
                            Value::Empty => self.push_op(VmOp::Eval(first)),
                            _ => {
                                self.push_op(VmOp::Apply(Value::from(SpecialForm::And(rest))));
                                self.push_op(VmOp::Eval(first));
                            }
                        }
                    }
                } else {
                    self.push_res(cond);
                }
            }
            SpecialForm::Or(args) => {
                let cond = self.pop_res();
                if !cond.is_true() {
                    if let Value::Empty = args {
                        self.push_res(Value::Bool(false));
                    } else {
                        let (first, rest) = utils::rest_take_1(&args)
                            .expect("should have at least one val if not Empty");
                        match rest {
                            Value::Empty => self.push_op(VmOp::Eval(first)),
                            _ => {
                                self.push_op(VmOp::Apply(Value::from(SpecialForm::Or(rest))));
                                self.push_op(VmOp::Eval(first));
                            }
                        }
                    }
                } else {
                    self.push_res(cond);
                }
            }
            SpecialForm::Cond(body, arrow, args) => {
                let cond = self.pop_res();
                if cond.is_true() {
                    if arrow {
                        let expr = utils::fixed_take_1(&body).unwrap();
                        self.push_res(cond);
                        self.push_op(VmOp::ApplyRes);
                        self.push_op(VmOp::Eval(expr));
                    } else {
                        self.eval_body(&body);
                    }
                } else if args == Value::Empty {
                    self.push_res(Value::Empty);
                } else {
                    let (first, rest) = utils::rest_take_1(&args).expect("should not be empty");
                    let (test, arrow, branch) = self.separate_cond_clause(first)?;
                    if test == Value::from(Str::from("else")) {
                        self.eval_body(&branch);
                    } else {
                        self.push_op(VmOp::Apply(Value::from(SpecialForm::Cond(
                            branch, arrow, rest,
                        ))));
                        self.push_op(VmOp::Eval(test));
                    }
                }
            }
            SpecialForm::Case(val, args) => match val {
                Value::Undefined => {
                    let case = self.pop_res();
                    self.push_op(VmOp::Apply(Value::from(SpecialForm::Case(case, args))));
                }
                _ => {
                    if args == Value::Empty {
                        self.push_res(Value::Empty);
                        return Ok(());
                    }

                    let (first, rest) = utils::rest_take_1(&args).unwrap();
                    let (cases, is_else, branch) = self.separate_case_clause(first.clone())?;
                    match Value::get_pair_cell(&cases) {
                        Some(cell) => {
                            if is_else
                                || cell
                                    .values()
                                    .any(|c| are_eqv(c, val.clone()).unwrap().is_true())
                            {
                                self.eval_body(&branch);
                            } else {
                                self.push_op(VmOp::Apply(Value::from(SpecialForm::Case(
                                    val, rest,
                                ))));
                            }
                        }
                        None => return Err(UserError::Syntax(first)),
                    }
                }
            },
        }
        Ok(())
    }

    fn separate_cond_clause(&self, expr: Value) -> Result<(Value, bool, Value), UserError> {
        match utils::rest_take_2(&expr) {
            Ok((first, second, rest)) => {
                if second == Value::from(Str::from("=>")) {
                    Ok((first, true, rest))
                } else {
                    Ok((first, false, Value::from(Cell::new(second, Some(rest)))))
                }
            }
            Err(_) => Err(UserError::Syntax(expr)),
        }
    }

    fn separate_case_clause(&self, expr: Value) -> Result<(Value, bool, Value), UserError> {
        match utils::rest_take_1(&expr) {
            Ok((first, rest)) => {
                if first == Value::from(Str::from("else")) {
                    Ok((Value::Empty, true, rest))
                } else {
                    Ok((first, false, rest))
                }
            }
            Err(_) => Err(UserError::Syntax(expr)),
        }
    }

    /*** Stack Helpers ***/

    fn init_stacks(&mut self) {
        self.op_stack = Stack::push(Stack::new_rc(VmOp::Halt, None), VmOp::Halt);
        self.res_stack = Stack::push(Stack::new_rc(Value::Empty, None), Value::Empty);
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

    fn pop_res(&mut self) -> Value {
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

    fn push_res(&mut self, val: Value) {
        self.res_stack = Stack::push(Rc::clone(&self.res_stack), val);
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
            Rc::new(Env::new_with_bindings(&[(Value::new_sym("A"), Value::new_int(5))]).unwrap());
        let mut vm = Vm::new(Rc::clone(&env));
        assert_eq!(vm.eval(Value::new_sym("A")), Ok(Value::new_int(5)));
        assert_eq!(
            vm.eval(Value::new_sym("B")),
            Err(Error::Undeclared("B".to_owned()))
        );
    }

    #[test]
    fn test_eval_simple_if() {
        let env = Rc::new(Env::new());
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(if #t 1 0)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(1)));

        let expr = StringReader::new("(if #f 1 0)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(0)));

        let expr = StringReader::new("(if #t 1)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(1)));

        let expr = StringReader::new("(if #f 1)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Empty));
    }

    #[test]
    fn test_eval_builtin() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(cons 1 2)").read().unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(Value::new_int(1), Value::new_int(2)))
        );
    }

    #[test]
    fn test_eval_quote() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(cons 1 '(2))").read().unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(
                Value::new_int(1),
                Value::new_pair(Value::new_int(2), Value::Empty)
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
            Ok(Value::Closure(Rc::new(Closure::new(
                "no-name",
                Rc::clone(&env),
                Formals::Fixed(vec![Value::new_sym("x")]),
                vec![Value::new_int(1), Value::new_int(2)]
            ))))
        );
    }

    #[test]
    fn test_apply_lambda() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("((lambda () 2))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(2)));

        let expr = StringReader::new("((lambda (x) (cons x 2)) 1)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(Value::new_int(1), Value::new_int(2)))
        );

        let expr = StringReader::new("((lambda (x) 77 88 (cons x 2)) 1)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(Value::new_int(1), Value::new_int(2)))
        );

        let expr = StringReader::new("((lambda (x) (cons x 2)) 1 2 3 4)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(Value::new_int(1), Value::new_int(2)))
        );

        let expr = StringReader::new("((lambda x (cons 1 x)) 2 3 4)")
            .read()
            .unwrap();
        assert_eq!(
            vm.eval(expr),
            Ok(Value::cons(
                Value::new_int(1),
                Value::cons(
                    Value::new_int(2),
                    Value::cons(
                        Value::new_int(3),
                        Value::cons(Value::new_int(4), Value::Empty)
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
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(let ((a 5) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(11)));

        let expr = StringReader::new("(let ((a (+ 3 4)) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(13)));

        let expr = StringReader::new("(let ((f (lambda (x) (+ x 1))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(7)));
    }

    #[test]
    fn test_eval_let_star() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(let* () 1 2 3 4 5)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(let* ((a 5)) a)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(let* ((a 5) (b 6)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(11)));

        let expr = StringReader::new("(let* ((a (+ 3 4)) (b a)) 77 88 (+ a b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(14)));

        let expr = StringReader::new("(let* ((a 2) (f (lambda (x) (+ x a))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(8)));
    }

    #[test]
    fn test_eval_letrec() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(letrec ((a 5)) a)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(letrec ((f (lambda (x) (+ x 1))) (b 6)) (f b))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(7)));

        let expr = StringReader::new(
            "(letrec ((f (lambda (x) (g x 1))) (g (lambda (x y) (- x y))) (b 10)) (f b))",
        )
        .read()
        .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(9)));
    }

    #[test]
    fn test_eval_begin() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(begin (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(12)));
    }

    #[test]
    fn test_eval_and() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(and (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(12)));

        let expr = StringReader::new("(and (+ 1 2) (- 1 2) #f (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Boolean(false)));

        let expr = StringReader::new("(and)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Boolean(true)));

        let expr = StringReader::new("(let ((a 10)) (and (set! a 5) a))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));
    }

    #[test]
    fn test_eval_or() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(or (+ 1 2) (- 1 2) (/ 3 4) (* 3 4))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(3)));

        let expr = StringReader::new("(or #f (= 1 3) (< 99 1))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Boolean(false)));

        let expr = StringReader::new("(or #f (* 3 4))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(12)));

        let expr = StringReader::new("(or)").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Boolean(false)));

        let expr = StringReader::new("(let ((a 10)) (or (set! a 5) (set! a 20)) a)")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));
    }

    #[test]
    fn test_eval_do() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(do ((i 0 (+ i 1))) ((= i 5) (+ i 5)))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(10)));

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
            Ok(Value::new_vec_mut(vec![
                Value::new_int(0),
                Value::new_int(1),
                Value::new_int(2),
                Value::new_int(3),
                Value::new_int(4),
            ]))
        );
    }

    #[test]
    fn eval_multiple_forms() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let forms = StringReader::new("(+ 1 2) (+ 5 6)").read_forms().unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(Value::new_int(11)));
    }

    #[test]
    fn test_eval_define() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let forms = StringReader::new("(define (f) 13) (f)")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(Value::new_int(13)));

        let forms = StringReader::new("(define a 5)").read_forms().unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(Value::Empty));

        let forms = StringReader::new("(define a 5) (+ a 10)")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&forms), Ok(Value::new_int(15)));

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
            Ok(Value::new_vec_mut(vec![
                Value::new_int(0),
                Value::new_int(1),
                Value::new_int(2),
                Value::new_int(3),
                Value::new_int(4),
            ]))
        );

        let forms = StringReader::new("(define (f . x) (cons 1 x)) (f 2)")
            .read_forms()
            .unwrap();
        assert_eq!(
            vm.eval_forms(&forms),
            Ok(Value::cons(
                Value::new_int(1),
                Value::cons(Value::new_int(2), Value::Empty)
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
        assert_eq!(vm.eval(expr), Ok(Value::new_int(7)));

        let expr = StringReader::new("(cond (#f 7) (#t 9) (#t 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(9)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#t 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#t 1 2 3 4 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(5)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (#f 5))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::Empty));

        let expr = StringReader::new("(cond (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(33)));

        let expr = StringReader::new("(cond (#f 7) (#f 9) (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(33)));

        let expr = StringReader::new("(cond ((+ 1 2) => -) (#f 9) (else 1 2 3 4 33))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(-3)));
    }

    #[test]
    fn test_eval_case() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(case (+ 2 1) ((1 2 3) 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_sym("a")));

        let expr = StringReader::new("(case (+ 2 3) ((1 2 3) 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_sym("b")));

        let expr = StringReader::new("(case (+ 2 3) ((1 2 3) 'a) ((a b c) 'b) (else 1 2 3 4 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_sym("c")));

        let expr = StringReader::new("(case (+ 2 1) (() 'a) ((4 5 6) 'b) (else 'c))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_sym("c")));
    }

    #[test]
    fn test_eval_user_apply() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(apply + '(1 2 3 4))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(10)));

        let expr = StringReader::new("(apply + 1 2 3 '(5))").read().unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(11)));

        let expr = StringReader::new("(apply + 1 2 (+ 1 2) (cons 6 '())))")
            .read()
            .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(12)));

        let expr = StringReader::new(
            "(let ((compose (lambda (f g)
                              (lambda args
                                (f (apply g args))))))
               ((compose - +) 1 2 3 4))",
        )
        .read()
        .unwrap();
        assert_eq!(vm.eval(expr), Ok(Value::new_int(-10)));
    }

    #[test]
    fn test_eval_user_eval() {
        let env = Env::new_null_rc();
        let mut vm = Vm::new(Rc::clone(&env));

        let expr = StringReader::new("(define + -) (eval '(+ 1 2) (null-environment))")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&expr), Ok(Value::new_int(3)));

        let expr = StringReader::new("(define + -) (eval '(+ 1 2))")
            .read_forms()
            .unwrap();
        assert_eq!(vm.eval_forms(&expr), Ok(Value::new_int(-1)));
    }
}
