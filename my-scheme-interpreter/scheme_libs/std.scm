;; my-scheme core library procedures ;;

;; Equality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO this is not implemented properly yet
(define equal?
  (lambda (a b)
    (eqv? a b)))


;; Lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (null? x) (eqv? x '()))
(define (list . xs) xs)

;; Because errors do not propogate and list-ref uses list-tail this
;; is list-tail but with an additional name parameter to give the right error
;; when out of boounds.
(define (list-tail-helper xs idx name)
  (letrec ((helper-rec
             (lambda (x i)
               (cond
                 ((eqv? i 0)
                  (if (null? x)
                    (range-error! name idx xs)
                    x))
                 ((not (pair? x)) (range-error! name idx xs))
                 (else (helper-rec (cdr x) (- i 1)))))))
    (helper-rec xs idx)))
  

(define (list-tail xs idx)
  (list-tail-helper xs idx 'list-tail))

(define (list-ref xs idx)
  (let ((tail (list-tail-helper xs idx 'list-ref)))
    (if (pair? tail)
      (car tail)
      (arg-type-error! 'list-ref tail "proper list"))))


;; Extra car,cdr, etc. helpers
(define caar (lambda (x) (car (car x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))

(define cddr (lambda (x) (cdr (cdr x))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))));

(define cadr (lambda (x) (car (cdr x))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))

(define caadr (lambda (x) (car (car (cdr x)))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))

(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))

(define caaddr (lambda (x) (car (car (cdr (cdr x))))))


;; Bool ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (not x) (if x #f #t))

;; Characters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char=? a b) (= (char->integer a) (char->integer b)))
(define (char<? a b) (< (char->integer a) (char->integer b)))
(define (char>? a b) (> (char->integer a) (char->integer b)))
(define (char<=? a b) (<= (char->integer a) (char->integer b)))
(define (char>=? a b) (>= (char->integer a) (char->integer b)))

(define (char-ci=? a b)
  (= (char->integer (char-downcase a))
     (char->integer (char-downcase b))))
(define (char-ci<? a b)
  (< (char->integer (char-downcase a))
     (char->integer (char-downcase b))))
(define (char-ci>? a b)
  (> (char->integer (char-downcase a))
     (char->integer (char-downcase b))))
(define (char-ci<=? a b)
  (<= (char->integer (char-downcase a))
      (char->integer (char-downcase b))))
(define (char-ci>=? a b)
  (>= (char->integer (char-downcase a))
      (char->integer (char-downcase b))))
