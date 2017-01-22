#lang racket

(define (sum-coins pennies nickels dimes quarters)
  (+ (+ pennies (* nickels 5)) (+ (+(* dimes 10) (* quarters 25)))))


(+ 42 (if(string? "hello") (+ 1 2) (/ 1 2)));an if statement

;cond
(define (foo x)
  (cond
    [(number? x) (+ x 1)]
    ((string? x) (+ (string->number x) 1))
    [else 0]
    ))

;how we evaluate stuff
(if #t (+ 1 2) (/ 5 0))
(and #f (/ 5 0))
(or #t (/ 5 0))
(and 42 2 3 #t 5);look at all of em. if all true, evaluate the last one

(and "ru" "n")


;lists. the ' means dont evaluate it, it's a list. All here is a list with some symbols in it.
'(1 2 3 4)
'(1 #t "hello")
'(1 (+ 2 3) 4 5)


(first '(1 2 3 4))
(rest '(1 2 3 4))
( list 1 2 3 4)
(list 1 (+ 2 3) 4 5)

;print is a function

(list 1 2 3 (list 4 5 6))
(first (rest (rest (rest bar ))))
