const initial_code = `(define counter
  (lambda ()
    (let ((x 0))
      (lambda ()
        (set! x (+ x 1))
        x))))
(define f (counter))
(define g (counter))

(f)
(f)
(g)
(+ (f) (g))
`

const out_header = "RESULT>  ";

// Clear the input area
function clear_text() {
  document.getElementById("code_input").value = "";
  document.getElementById("eval_result").textContent = out_header;
};

// Give the text area a default expression
function reset_text() {
  document.getElementById("code_input").value = initial_code;
  document.getElementById("eval_result").textContent = out_header;
};


// Interpret the string in the input area
// my eval is loaded from the wasm module and is not available without it
function interpret() {
  let code = document.getElementById("code_input").value;
  document.getElementById("eval_result").textContent = out_header + my_eval(code);
};

reset_text();
