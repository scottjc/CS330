#lang racket
;higher-orger function
;n order of a function, not a language

(define (add-one lst)
  (if (empty? lst)
      '()
      (cons (+1 (first lst)) (add-one (rest lst)))))

(define (add-const lst x)
  (if (empty? lst)
      '()
      (cons (+ x (first lst)) (add-const (rest lst) x))))
;we can take this and make it more general
;multiply const
;op-const is a higher order function because it accepts variable and functions and uses em
;function with < > = stuff
;f o g = f(g(x)) compose. It's an example of a higher order function


;$$$$$$$$$$$$$$$$$
;FILTER when you put something in it, it returns things that are true
(filter even? (list 1 2 3 4)) ;'(2 4)

(define (inc x ) (+ x 1 ))
;MAP goes through a list easily. It goes left to right. only 1 arg
(map inc (list 1 2 3 4)) ;'(2 3 4 5) 

;fold r and fold l. Binary operators. Got left or right.
(foldr + 0 (list 1 2 3 4)) ;r -2 l 2?????????????????????????????


;google stuff a map followed by a reduce
(define (myand x y) (and x y))
(define (myor x y) (or x y))

(define (map-reduce lst mapop foldbase foldop)
  (foldr foldop foldbase (map mapop lst)))

(map-reduce (list 1 2 3 4 5 6) even? #t myand)

(map-reduce (list 1 2 3 4 5 6) even? #f myor)



;compose put results together ?????????????????????????????????
(define sq-inc2 (compose (lambda (x) (* x x )) (lambda (x) (+ 1 x))))
;(sq-inc2 42)) 1849

;curying   (inc-by 5) a function. add 5
(define (inc-by k)
  (lambda (x)
    (= x k)))