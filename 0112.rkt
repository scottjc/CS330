#lang racket
(define (extract str);prefix notation
  (substring str 4 7))

;01/01/2017
;a symbol in an interpretted language. No value assigned to it. Nothing is
;   bound to it.
'abc

;true and false
#t
#f
(>= 5 0) ;true
(negative? 3) ;false
(string=? "hello" "hello") ;true
(number->string 42)
(string-> number "42")


(define foo 42)

;'+
;eval foo
;eval we evaluate this expressioin. if symbol, look it up

;we can pass functions around!

;symbol / vs. procedure /

;STUFF FROM LAB 1!
;My tests
(define (round-it num place)
  (let ((power (expt 10 place)))
    (/ (round (* power num)) power)))
(require rackunit)
(check-eq? (sum-coins 1 1 1 1) 41)
(check-eqv? (round-it (degrees-to-radians 60) 3) 1.047)
(check-eq? (sign 4) 1)
(check-eqv? (round-it (new-sin 60 'radians) 3) -0.305)


(require rackunit)
(check-eq? (sum-coins 2 1 1 2) 67)
(check-eq? (sum-coins 56 0 0 0) 56)

(check-eqv? (round-it (degrees-to-radians 160) 3) 2.793)
(check-eqv? (round-it (degrees-to-radians 8) 4) 0.1396)

(check-eq? (sign 56) 1)
(check-eq? (sign -756) -1)
(check-eq? (sign 0) 0)

(check-eqv? (round-it (new-sin 80 'radians) 3) -0.994)
(check-eqv? (round-it (new-sin 70 'degrees) 2) 0.94)