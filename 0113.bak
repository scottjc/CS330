#lang racket

;#1
;This WORKS!!!!
(define (sum-coins pennies nickels dimes quarters)
  (+ (+ pennies (* nickels 5)) (+ (+(* dimes 10) (* quarters 25)))))

;#2
;This works. Do we need pi to be better?
(define conv (/ pi 180))
(define (degrees-to-radians angle)
  (* angle conv))
   
;#3
;This WORKS!!!!
(define (sign x)
  (cond 
    [(negative? x) -1]
    [(positive? x)  1]
    [else 0]))
  
;#4
; Because sin in meant for doing radians
(define (new-sin angle type)
  (cond
  [(symbol=? type 'degrees)(sin (degrees-to-radians angle))]
  [(symbol=? type 'radians)(sin angle)]))
