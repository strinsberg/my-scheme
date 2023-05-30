;; my-scheme core library procedures ;;

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

;; Characters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

