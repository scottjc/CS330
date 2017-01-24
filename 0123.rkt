#lang racket

(define k 42)

(define (inc-by k)
  (lambda (x)(+ x k)))

;partial eveluation. multivar define some, leave ofthers free.
(define(4list a b c d)
  (list a b c d));2 list 2 params, calls 4 list and last 2 params are fixed
(define (2list c d)
  (lambda (a b)
  (4list a b c d)));we don't know all the params when we start
;((2list 42 16)1 2)) = '(1 2 42 16)


;curying. Reduced dimentionality of functions
(define (curry1 onefunc)
  (lambda (x) (onefunc x)))
;take arguments and pack it into a list

(define (apply-with-const2 lst op k);WWWWWWWWWWWWWWWWWOOOOOOOOOOOOOOOWWWWWWWW!!
  (map (lambda (x) (op x k)) lst))


;decorators
