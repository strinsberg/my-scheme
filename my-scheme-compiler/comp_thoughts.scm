;; Not code, just some notes on how I might compile scheme to rust using the
;; data structures that I have created for the interpreter

;; symbol
a => Value::from ("a")

;; number
1 => Value::from (1)
1.0 => Value::from (1.0)
1/2 => Value::from (Num::new_rat (1, 2))

;; quotes can be resolved at compile time by not evaluating their arguments
;; cons, car, cdr
(cons 1 2) => core::cons (Value::from (1), Value::from (2))
(car '(1 2)) => core::car (list! [Value::from (1), Value::from (2)])
(car '(1 2)) => core::cdr (list! [Value::from (1), Value::from (2)])

;; if is an expression in rust
(if #t 1 2) => if Value::from (true).is_true () { Value::from (1) } else { Value::from (2) }

;; lambda is a little trickier because we need to both create a lambda data
;; structure and call that structure, but the body must be compiled into something
;; that we can call in rust as we will not interprete it.
(define inc (lambda (x) (+ x 1))) =>
env.insert (Str::from ("inc"), Value::Undefined)
env.set (Str::from ("inc"),
         core::lambda (Some ("inc") ,
                       Formals::Fixed (vec! [ Value::sym_from_str ("x")]),
                       env.clone (),
                       |env| proc::sum (vec! [env.get (Str::from ("x")), Value::from (1)])))

(inc 5) => Value::apply (env.get (Value::sym_from_str ("inc")) , (vec! [Value::from (5)]))

(inc (+ 1 2)) => Value::apply (
                  env.get (Value::sym_from_str ("inc")) ,
                  vec! [proc::sum (vec! [Value::from (1), Value::from (2)])])

pub fn Value::apply (closure: Value, args: Vec<Value>) -> Result<Value, Error> {
    let closure = Value::get_closure (&closure)?
    let env = bind_closure_args (env, closure.params, args)
    closure.call (env)
}

closure::call (env) {
    let func = self.func
    func (env)
}

;; What we learn here is that symbols that have builtin procedures can have
;; names from a lib that are inserted. All procs take a vector or values and
;; all closure applications take a vector of values and a value that must be
;; a closure. Everything returns a value or error. Closures are the same as
;; they are now except for the function pointer. Let etc. can still be transformed
;; to lambdas at compile time and get compiled as lambda applications.
;; Everything in rust is an expression so we can compile things very much like lisp
;; and they will look like bad rust code, but it will save storing results in
;; intermediate values and then substituting in variable names. It will in a way
;; resemble the interpreters code for builtin procs, but without the need to
;; interpret it.

;; Some experiments could be made copying over code from the interpreter and adjusting
;; int to work with the compiled code. The right way to do it is to wrap the existing
;; functions in functions that take vectors of arguments, just like the proc
;; vecs wrap them to take lists. We just have to make sure they do not use the
;; list functions internally on things that might be vectors. The early way to try this
;; is to just make a rust project with a main file and write the compiled code
;; manually to see if it works. Then it can be expanded to use the reader from
;; the lib to generate a syntax tree that we can write rules for compilation from.

;; I think that the things that will be missing or difficult to include will be
;; eval and things like continuations. However, I could be OK with that. I think
;; continuations could probably be implemented with goto if it exists in rust,
;; the env saving is easy because when we create a continuation we can save the env
;; easy enough. The stack state I am not sure about.
