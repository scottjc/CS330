#lang racket
;cons adds stuff to lists in the front
(cons 42 '(43))

(define (lengthh lst)
  (if (empty? lst)
   0
   (+ 1 (lengthh (rest lst)))))

;set of c++ programs. Syntactically valid vs will compile
;grammar: mechanism to descripbe languages buffalo^n

(define (all-even? lst)
  (if (empty? lst);base
      #t ;identity element
      (if (even? (first lst))
          (all-even? (rest lst))
          #f)))

;are-even

;in racket, functions are first objects.
;