#lang racket

(define L1 '(66 67 68 69))
(define L2 '(66 67 68 99 69))
(define L3 '(0 -2 3 99 100 999 101))
(define T1 '(110 100 -20))
(define LL1 '(1 2 3))
;fold returns a number
;fold lambda (list))=t
;(xy)(and 96>x>5 y)
;map iterate and apply and op. get list back
;andmap / but and
;fold iterate return one num var

;check-temps andmap DONE
(define (check-temps temps low high)
(andmap (lambda (x) (and
                         (< x high)
                         (> x low)))temps))



;check-temp1 andmap DONE
(define (check-temps1 temps)
  (check-temps temps 5 95))



;convert foldr DONE
(define (convert digits)
  (foldr + 0 (convertREC digits 1)))

(define (convertREC digits mult);basically the same function from Lab2, but we map the numbers together
  (if (empty? digits);base case
      (list 0);starting number
      (append (list (* (first digits) mult)) (convertREC (rest digits) (* mult 10)))))


  
;duple map DONE
(define (duple lst)
  (map dupleREC lst))

(define (dupleREC item)
  (cons item (list item)));add a item to the stack in a list



;average foldl DONE
(define (average lst)
  (/ (foldl (lambda (x y) (+ x y)) 0 lst ) (length lst)))



;convertFC map DONE
(define (convertFC temps)
   (map (lambda (x) (* (- x 32) (/ 5 9))) temps))



;eliminate-larger filter DONE
(define (eliminate-larger lst)
  (foldr ;move from the right to the left
    (lambda (member before)
       (if (andmap (lambda (n) (<= member n)) before);is it less than or = to the right one
         (cons member before);add it to the list
         before));the number before it
    '() lst));start with an empty list and the list itself



;curry2 DONE
;store in variables and functions as we get em
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))