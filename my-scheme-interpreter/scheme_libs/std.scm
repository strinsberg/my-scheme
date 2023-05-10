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
(define (__list-tail-helper__ xs idx name)
  (letrec ((helper-rec
             (lambda (x i)
               (cond
                 ((= i 0)
                  (if (null? x)
                    (range-error! name idx xs)
                    x))
                 ((not (pair? x)) (range-error! name idx xs))
                 (else (helper-rec (cdr x) (- i 1)))))))
    (helper-rec xs idx)))
  

(define (list-tail xs idx)
  (__list-tail-helper__ xs idx 'list-tail))

(define (list-ref xs idx)
  (let ((tail (__list-tail-helper__ xs idx 'list-ref)))
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

(define (__char-compare a b f) (f (char->integer a) (char->integer b)))
(define (__char-compare-ci a b f)
  (f (char->integer (char-downcase a))
     (char->integer (char-downcase b))))

(define (char=? a b) (__char-compare a b =))
(define (char<? a b) (__char-compare a b <))
(define (char>? a b) (__char-compare a b >))
(define (char<=? a b) (__char-compare a b <=))
(define (char>=? a b) (__char-compare a b >=))

(define (char-ci=? a b) (__char-compare-ci a b =))
(define (char-ci<? a b) (__char-compare-ci a b <))
(define (char-ci>? a b) (__char-compare-ci a b >))
(define (char-ci<=? a b) (__char-compare-ci a b <=))
(define (char-ci>=? a b) (__char-compare-ci a b >=))

;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Equality ;;
(define (__string-compare-eq__ a b f)
  (if (= (string-length a) (string-length b))
    (letrec ((len (string-length a))
             (helper
               (lambda (i a b)
                 (cond
                   ((>= i len) #t)
                   ((not (f (string-ref a i) (string-ref b i))) #f)
                   (else (helper (+ i 1) a b))))))
      (helper 0 a b))
    #f))

(define (string=? a b) (__string-compare-eq__ a b char=?))
(define (string-ci=? a b) (__string-compare-eq__ a b char-ci=?))

;; Ordering ;;
(define (__string-compare-ord__ a b f e)
  (letrec ((len-a (string-length a))
           (len-b (string-length b))
           (helper
             (lambda (i a b)
               (cond
                 ((and (>= i len-a) (>= i len-b)) e)
                 ((and (>= i len-a) (< i len-b)) #t)
                 ((>= i len-b) #t)
                 ((f (string-ref a i) (string-ref b i)) #t)
                 (else (helper (+ i 1) a b))))))
    (helper 0 a b)))

(define (string<? a b) (__string-compare-ord__ a b char<? #f))
(define (string>? a b) (__string-compare-ord__ a b char>? #f))
(define (string<=? a b) (__string-compare-ord__ a b char<=? #t))
(define (string>=? a b) (__string-compare-ord__ a b char>=? #t))

(define (string-ci<? a b) (__string-compare-ord__ a b char-ci<? #f))
(define (string-ci>? a b) (__string-compare-ord__ a b char-ci>? #f))
(define (string-ci<=? a b) (__string-compare-ord__ a b char-ci<=? #t))
(define (string-ci>=? a b) (__string-compare-ord__ a b char-ci>=? #t))
