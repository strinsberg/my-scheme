;; my-scheme core library procedures ;;


;; Equality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO this is not implemented properly yet
(define equal?
  (lambda (a b)
    (eqv? a b)))

;; Functional ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This might be optimizable still, but this is a pretty standard way of
;; doing a map and a little tricky when you have to have multiple lists to
;; apply it to. Also, does not check for arity or that all lists are the same
;; size.
(define (map f . lists)
  (letrec ((map-single
             (lambda (f xs acc)
                (if (null? xs)
                  (reverse acc)
                  (map-single f
                              (cdr xs)
                              (cons (f (car xs)) acc)))))
           (map-multi
             (lambda (f lists acc)
               (if (null? (car lists))
                 (reverse acc)
                 (map-multi f
                            (map-single cdr lists '())
                            (cons (apply f (map-single car lists '()))
                                  acc))))))
    (map-multi f lists '())))

;; Same strategy as map, but with no accumulators and we return '()
(define (for-each f . lists)
  (letrec ((for-each-single
             (lambda (f xs)
                (if (null? xs)
                  '()
                  (begin
                    (f (car xs))
                    (for-each-single f (cdr xs))))))
           (for-each-multi
             (lambda (f lists)
               (if (null? (car lists))
                 '()
                 (begin
                   (apply f (map car lists))
                   (for-each-multi f (map cdr lists)))))))
    (for-each-multi f lists)))

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

(define (string . xs)
    (do ((new-string (make-string (length xs)))
         (i 0 (+ i 1))
         (ls xs (cdr ls)))
        ((null? ls) new-string)
      (string-set! new-string i (car ls))))

;; This is a kind of naive way of doing this, it is probably possible to get
;; the total length of each of the strings and make a new string that long
;; and then take each string and copy each char to the new string in the
;; right place. I am not sure how much more efficient it would but it would
;; could reduce us from 3 iterations over each string to 1. Maybe also a string
;; push could make it easy and more efficient. Then we could make an str function
;; like clojure that turns every value into a string and appends them together.
;; we could also make a string join quite easily too with string-push.
(define (string-append . strings)
  (apply string
    (apply append
           (map string->list strings))))

(define (string->list str)
  (let ((len (string-length str)))
    (do ((i (- len 1) (- i 1))
         (ls (list) (cons (string-ref str i) ls)))
        ((= i -1) ls))))

(define (list->string xs) (apply string xs))

(define (substring str start end)
    (do ((new-str (make-string (- end start)))
         (i start (+ i 1)))
        ((= i end) new-str)
      (string-set! new-str (- i start) (string-ref str i))))

(define (string-copy str)
  (let ((len (string-length str)))
    (do ((new-str (make-string len))
         (i 0 (+ i 1)))
        ((= i len) new-str)
      (string-set! new-str i (string-ref str i)))))

(define (string-fill! str ch)
  (do ((len (string-length str))
       (i 0 (+ i 1)))
      ((= i len) '())
    (string-set! str i ch)))

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

;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->vector xs) (apply vector xs))

(define (vector->list vec)
  (let ((len (vector-length vec)))
    (do ((i (- len 1) (- i 1))
         (ls (list) (cons (vector-ref vec i) ls)))
        ((= i -1) ls))))

(define (vector-fill! vec obj)
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len) '())
    (vector-set! vec i obj)))

;; Just an extra function I cooked up for something I did not need it for.
;; Maybe not robust yet, but it has a test and works for a standard case.
(define (vector-transpose V)
  ;; Still need to ensure all vectors are the same length, but it will error
  ;; anyway if they are not
  (let ((m (vector-length V))
        (n (vector-length (vector-ref V 0))))
    (do ((i 0 (+ i 1))
         (outer (make-vector n)))
        ((= i n) outer)
      (do ((j 0 (+ j 1))
           (inner (make-vector m)))
          ((= j m) (vector-set! outer i inner))
        (vector-set! inner j (vector-ref (vector-ref V j) i))))))
