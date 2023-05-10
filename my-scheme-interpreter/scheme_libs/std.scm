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

;; These comparisson functions are a little gross, but they save having to
;; write the same function 10 times. The extra parameters on the ordered ones
;; determine what to return in cases where we get to the end of one or both or
;; the strings.

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

;; Ordered ;;
(define (__string-compare-ord__ a b f a-larger b-larger)
  (letrec ((len-a (string-length a))
           (len-b (string-length b))
           (helper
             (lambda (i a b)
               (cond
                 ((and (>= i len-a) (>= i len-b)) #f)
                 ((and (>= i len-a) (< i len-b)) a-larger)
                 ((>= i len-b) b-larger)
                 ((f (string-ref a i) (string-ref b i)) #t)
                 (else (helper (+ i 1) a b))))))
    (helper 0 a b)))

(define (string<? a b) (__string-compare-ord__ a b char<? #t #f))
(define (string>? a b) (__string-compare-ord__ a b char>? #f #t))
(define (string-ci<? a b) (__string-compare-ord__ a b char-ci<? #t #f))
(define (string-ci>? a b) (__string-compare-ord__ a b char-ci>? #f #t))

;; Ordered or Equal ;;
(define (__string-compare-ord-eq__ a b gt eq a-larger b-larger)
  (letrec ((len-a (string-length a))
           (len-b (string-length b))
           (helper
             (lambda (i a b)
               (cond
                 ((and (>= i len-a) (>= i len-b)) #t)
                 ((and (>= i len-a) (< i len-b)) a-larger)
                 ((>= i len-b) b-larger)
                 ((gt (string-ref a i) (string-ref b i)) #t)
                 ((not (eq (string-ref a i) (string-ref b i))) #f)
                 (else (helper (+ i 1) a b))))))
    (helper 0 a b)))

(define (string<=? a b) (__string-compare-ord-eq__ a b char<? char=? #t #f))
(define (string>=? a b) (__string-compare-ord-eq__ a b char>? char=? #f #t))
(define (string-ci<=? a b) (__string-compare-ord-eq__ a b char-ci<? char-ci=? #t #f))
(define (string-ci>=? a b) (__string-compare-ord-eq__ a b char-ci>? char-ci=? #f #t))

;; String requires counting the list of arguments and making a new string with
;; the necessary length. Then just loop again and set string positions to the
;; correct ones. This and all those below would be much easier with
;; for-each, map, or reduce and the addition of a string-push function to add
;; chars to the end of the string.

;; For string append make a new string the same length as all the arguments
;; then maintain a char idx and for each string loop through it and string-set!
;; the position in the new string. requires string-set! and make-string.

;; String to list is just loop through the chars of the string and cons each of
;; them onto a list. Doing this with a recursive helper will make it easier,
;; but the string will have to be traversed backwards that way to keep it
;; tail recursive and not have to call reverse on the list.

;; List to string is easy again, just make a new string and maintain an index
;; as you loop through the list to string-set! the chars in the string.

;; Substring is the same as the others. Make a new string of the desired length
;; and loop through the string and set the appropriate chars in the new string.

;; String copy again is just loop through the string and set chars in the new
;; string as needed.
