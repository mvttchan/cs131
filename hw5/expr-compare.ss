#lang racket
(provide (all-defined-out))

(define LAMBDA (string->symbol "\u03BB"))

(define (LAMBDA_checker x)
  (if (eqv? LAMBDA (car x)) #t #f))

(define (lambda_checker x)
  (if (eqv? 'lambda (car x)) #t #f))

(define (quote_checker x)
  (if (eqv? 'quote (car x)) #t #f))

(define (base_case x y)
  (if (or (eqv? x '()) (eqv? y '()))
      '()
      (cons (expr-compare (car x) (car y)) (base_case (cdr x) (cdr y)))))

(define (trivial_compare x y)
  (if (and (boolean? x) (boolean? y))
      (if (equal? x y)
          x
          (if x '% '(not %)))
      (if (equal? x y) x `(if % ,x ,y))))

(define (lambda_helper x y)
  (cond
    [(or (empty? x) (empty? y)) (trivial_compare x y)]
    [(and (list? x) (list? y))
     (cond
       [(equal? (car x) (car y)) (cons (car x) (expr-compare (cdr x) (cdr y)))]
       [else (append `(if % ,(car x) ,(car y)) (expr-compare (cdr x) (cdr y)))])]
    [else (trivial_compare x y)]))

(define (lambda_compare x y symbol)
  (let ((x_arg (cadr x)) (y_arg (cadr y)) (x_tail (caddr x)) (y_tail (caddr y)))
    (if (list? x_tail)
        (cons (if symbol LAMBDA 'lambda)
                (append (list (lambda_helper x_arg y_arg))
                        (list (lambda_helper x_tail y_tail))))
        (cons (if symbol LAMBDA 'lambda)
                (append (list(lambda_helper x_arg y_arg))
                        (lambda_helper x_tail y_tail))))))
  
(define (equal_case x y)
  (cond
    [(quote_checker  x) (trivial_compare x y)]  ;cant parse quotes
    [(LAMBDA_checker x) (lambda_compare x y #t)];if special character
    [(lambda_checker x) (lambda_compare x y #f)];if string
    [else (base_case x y)]))

(define (not_equal_case x y)
  (cond;same conditions as above, added if case because the other might be a function
   [(or (quote_checker x)    (quote_checker y))    (trivial_compare x y)]
   [(or (equal? (car x) 'if) (equal? (car y) 'if)) (trivial_compare x y)]
   [(and (or (LAMBDA_checker x) (LAMBDA_checker y))
         (or (lambda_checker x) (lambda_checker y)))
         (cond
           [(or (LAMBDA_checker x) (LAMBDA_checker y)) lambda_compare x y #t]
           [else (lambda_compare x y #f)])]
   [else (base_case x y)]))

(define (expr-compare x y)
  (cond
    [(or (or (not (list? x)) (not (list? y)))  ;if not a list, or lists are different lengths
         (not (equal? (length x) (length y)))) (trivial_compare x y)]
    [(equal? x y) x]                           ;same lists
    [(equal? (car x) (car y)) (equal_case x y)];if start of list is same
    [else (not_equal_case x y)]))              ;if it isn't

(define (test-expr-compare x y)
	(and	(equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) 
       		(equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))