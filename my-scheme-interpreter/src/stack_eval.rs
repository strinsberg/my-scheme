use crate::core_proc as proc;
use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Closure, ConsCell, Env, Formals, ScmVal};
use std::cell::RefCell;
use std::rc::Rc;

// Stack //////////////////////////////////////////////////////////////////////
//
// Immutable shared Env style linked list to serve as an evaluation stack

#[derive(Debug, Clone, PartialEq)]
pub struct Stack {
    pub expr: ScmVal,
    pub env: Rc<RefCell<Env>>,
    pub next: Option<Rc<Stack>>,
}

impl Stack {
    pub fn new(expr: ScmVal, env: Rc<RefCell<Env>>, next: Option<Rc<Stack>>) -> Stack {
        Stack {
            expr: expr,
            env: env,
            next: next,
        }
    }

    pub fn new_rc(expr: ScmVal, env: Rc<RefCell<Env>>, next: Option<Rc<Stack>>) -> Rc<Stack> {
        Rc::new(Stack {
            expr: expr,
            env: env,
            next: next,
        })
    }

    pub fn push(stack: Rc<Stack>, expr: ScmVal, env: Rc<RefCell<Env>>) -> Rc<Stack> {
        Stack::new_rc(expr, env, Some(stack))
    }

    pub fn push_expr(stack: Rc<Stack>, expr: ScmVal) -> Rc<Stack> {
        Stack::new_rc(expr, stack.env.clone(), Some(stack))
    }

    pub fn copy_with(&self, expr: ScmVal, env: Rc<RefCell<Env>>) -> Rc<Stack> {
        Stack::new_rc(expr, env, self.next.clone())
    }

    pub fn copy_with_expr(&self, expr: ScmVal) -> Rc<Stack> {
        Stack::new_rc(expr, self.env.clone(), self.next.clone())
    }
}

// Eval ///////////////////////////////////////////////////////////////////////
//
// Evaluation with a stack to (hopefully) enable continuations.
//
// The current approach works for providing an unwind early mechanism, but
// it does not quite reach what might be expected. I.e. it will not return
// directly to the top and skip recursion unwinding. It will only allow the
// unwinding to happen before it might have been expected and if provided with
// a special stack skip other evaluations. The test_unwind shows this by putting
// an unwind as the condition to an if statment nested several times. When
// we hit unwind, even though it is the condition for the if we quit what we
// are doing and unwind back to the top ignoring what the if's would do. But
// again we are still very much unwinding rather than say just setting the
// stack pointer to an earlier location and proceeding from there. I think to
// do that appropriately one would have to have instrutions on the stack as well
// as arguments so that we could evaluate lists of arguments without recursion.
// This would allow simply replacing the stack with another stack pointer when
// unwinding allowing us to skip the recursions created by evaluating the arguments.
// The reason is that if we want to evaluate a list of arguments we have to push
// them to the stack, but save their results somewhere else until we can apply
// a procedure or keyword to them. For example with an if we would push the condition
// onto the stack to be evaluated and stored in the argument list. When this was
// done we would have an apply if instruction on the stack that would use the
// value in the args list and set the top of the stack as one branch and
// that would be evaluated and the result put on the stack. The only problem with
// what I describe here is that that final result would likely be some other
// procedures arguments and its evaluation would need to be placed on a fresh
// arg list and not the stack. Also, each function would need it's own arg
// list that would be like a stack as well. Which would make things more like a
// vm with an instruction stack and a result stack. I.e. any computation result
// would be added to a level on the result stack and any instruction would be
// placed on the instruction stack, while keeping track of the expression being
// evaluated to fill the instruction and result stacks.
//
// My idea for this is as follows:
//
// We have a vm that takes at least two states
// Eval, and Apply
// When eval we evaluate the form it refers to and push the result into a
// vector on a stack of results.
// When apply we pop a vector off the results stack and apply a special form,
// builtin, or procedure to it and push the result into the top of the result stack.
// At the end of evaluation we take the first(or last) result value (prob there should
// only ever be one when returning) and return it.
// The environment can be on a stack as well and when necessary a new one can be
// pushed or popped.
// A continuation can store a pointer to both stacks and maybe an environemnts
// and when applied just reset them all and put it's argument on top of the
// result stack.

pub fn eval(expr: ScmVal, env: Rc<RefCell<Env>>) -> ValResult {
    match eval_stack(Stack::new_rc(expr, env, None)) {
        Ok(s) => Ok(s.expr.clone()),
        Err(e) => Err(e),
    }
}

pub fn eval_stack(stack: Rc<Stack>) -> ScmResult<Rc<Stack>> {
    let mut stack = stack;

    loop {
        match stack.expr {
            ScmVal::Symbol(ref name) => {
                let value = match stack.env.borrow().lookup(stack.expr.clone()) {
                    Some(val) => val,
                    None => return Err(ScmErr::Undeclared(name.to_string())),
                };
                stack = stack.copy_with_expr(value);
            }
            ScmVal::Pair(ref cell) => match cell.head.clone() {
                ScmVal::Symbol(ref name) => match name.to_string().as_str() {
                    "if" => {
                        // eval condition, push which correct branch based on result
                        stack = eval_if(cell.tail.clone(), stack)?;
                    }
                    // This is just a temporary expression to try and see if unwind
                    // can work with this style of evaluator
                    "unwind" => {
                        stack = Stack::new_rc(cell.tail.clone(), stack.env.clone(), None);
                    }
                    _ => return Err(ScmErr::Syntax(stack.expr.clone())),
                },
                _ => return Err(ScmErr::Syntax(stack.expr.clone())),
            },
            _ => return Ok(stack),
        }

        stack = match stack.next {
            Some(ref s) => Rc::clone(s),
            None => return Ok(stack),
        };
    }
}

fn eval_if(args: ScmVal, stack: Rc<Stack>) -> ScmResult<Rc<Stack>> {
    let (arg_vec, _, _) = ScmVal::list_to_vec(args).unwrap();
    match arg_vec.len() {
        // (if cond true false)
        3.. => eval_if_helper(arg_vec, true, stack),
        // (if cond true)
        2 => eval_if_helper(arg_vec, false, stack),
        // has empty args or only cond
        _ => Err(ScmErr::Arity("if".to_owned(), 2)),
    }
}

// Helper to evaluate properly when given a false branch or not
fn eval_if_helper(args: Vec<ScmVal>, false_branch: bool, stack: Rc<Stack>) -> ScmResult<Rc<Stack>> {
    // check condition
    let result = eval_stack(Stack::push_expr(stack.clone(), args[0].clone()))?;

    if result.next != Some(stack.clone()) {
        return Ok(result);
    }

    // return the correct branch unevaluated
    let expr = match !proc::is_true(result.expr.clone()) {
        true => match false_branch {
            true => args[2].clone(),
            false => ScmVal::Empty,
        },
        false => args[1].clone(),
    };

    Ok(stack.copy_with_expr(expr))
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_symbol() {
        let env = Rc::new(RefCell::new(
            Env::new_with_bindings(vec![(ScmVal::new_sym("A"), ScmVal::new_int(5))]).unwrap(),
        ));
        assert_eq!(
            eval(ScmVal::new_sym("A"), Rc::clone(&env)),
            Ok(ScmVal::new_int(5))
        );
        assert_eq!(
            eval(ScmVal::new_sym("B"), Rc::clone(&env)),
            Err(ScmErr::Undeclared("B".to_owned()))
        );
    }

    #[test]
    fn test_eval_simple_if() {
        let env = Rc::new(RefCell::new(
            Env::new_with_bindings(vec![(ScmVal::new_sym("A"), ScmVal::new_int(5))]).unwrap(),
        ));

        let if_stmt = ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("if"),
                ScmVal::Boolean(true),
                ScmVal::new_int(1),
                ScmVal::new_int(0),
            ],
            ScmVal::Empty,
        );
        assert_eq!(eval(if_stmt, Rc::clone(&env)), Ok(ScmVal::new_int(1)));

        let if_stmt = ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("if"),
                ScmVal::Boolean(false),
                ScmVal::new_int(1),
                ScmVal::new_int(0),
            ],
            ScmVal::Empty,
        );
        assert_eq!(eval(if_stmt, Rc::clone(&env)), Ok(ScmVal::new_int(0)));

        let if_stmt = ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("if"),
                ScmVal::Boolean(true),
                ScmVal::new_int(1),
            ],
            ScmVal::Empty,
        );
        assert_eq!(eval(if_stmt, Rc::clone(&env)), Ok(ScmVal::new_int(1)));

        let if_stmt = ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("if"),
                ScmVal::Boolean(false),
                ScmVal::new_int(1),
            ],
            ScmVal::Empty,
        );
        assert_eq!(eval(if_stmt, Rc::clone(&env)), Ok(ScmVal::Empty));
    }

    #[test]
    fn test_unwind() {
        let env = Rc::new(RefCell::new(
            Env::new_with_bindings(vec![(ScmVal::new_sym("A"), ScmVal::new_int(5))]).unwrap(),
        ));

        let if_stmt = ScmVal::vec_to_list(
            vec![
                ScmVal::new_sym("if"),
                ScmVal::vec_to_list(
                    vec![
                        ScmVal::new_sym("if"),
                        ScmVal::vec_to_list(
                            vec![
                                ScmVal::new_sym("if"),
                                ScmVal::vec_to_list(
                                    vec![ScmVal::new_sym("unwind"), ScmVal::Boolean(true)],
                                    ScmVal::Empty,
                                ),
                                ScmVal::new_int(1),
                            ],
                            ScmVal::Empty,
                        ),
                        ScmVal::new_int(1),
                    ],
                    ScmVal::Empty,
                ),
                ScmVal::new_int(1),
                ScmVal::new_int(0),
            ],
            ScmVal::Empty,
        );
        assert_eq!(eval(if_stmt, Rc::clone(&env)), Ok(ScmVal::Boolean(true)));
    }
}
