#lang racket
(require math/statistics)

(define L1 '(66 67 68 69))
(define L2 '(66 67 68 99 69))
(define L3 '(0 -2 3 99 100 999 101))
(define T1 '(110 100 -20))
(define LL1 '(1 2 3))

;check-temps1 DONE
(define (check-temps1 temps)
  (if (empty? temps);base case
      true
      (and (cond
             ((> (first temps) 95) false)
             ((< (first temps) 5) false)
             (else (check-temps1 (rest temps)))))))


;check-temp DONE
(define (check-temps temps low high)
   (if (empty? temps);base case
      true
      (and (cond
             ((> (first temps) high) false)
             ((< (first temps) low) false)
             (else (check-temps (rest temps) low high))))))


;convert DONE
(define (convert digits)
  (convertREC digits 0 1))

(define (convertREC digits answer mult)
     (cond
      ((empty? digits) answer);base case
      (else
       (convertREC (rest digits) (+ answer (* mult (first digits))) (* 10 mult)))))


;duple DONE
(define (duple lst)
  ;reverse the list
  (dupleREC (reverse lst) '() ))

(define (dupleREC lst ret)
   (cond
      ((empty? lst) ret);base case
      (else
       ;(cons(# #) current-rec. Use list append for NOT PAIRS
       ;cons a a list of 2 numbers B ret)
       ;this line is put on the stack
       (dupleREC (rest lst) (cons (list (first lst) (first lst)) ret)))))


;average DONE
(define (average lst)
  (mean lst)) ;Is it ok to use 'mean'? It's pretty convinitent.
;HOW DO WE WANT IT DIVIDED?!?! 135/2 of 67.5?


;convertFC DONE
(define (convertFC temps)
 (convertFCrec (reverse temps) '() ))

(define (convertFCrec temps ntemps)
     (cond
      ((empty? temps) ntemps);base case
      (else
       (convertFCrec (rest temps) (cons (* (- (first temps) 32) (/ 5 9)) ntemps)))))


;eliminate-larger DONE
(define (eliminate-larger lst)
    (eliminate-largerREC (reverse lst) '() ))

(define (eliminate-largerREC lst nlst)
     (cond
      ((empty? lst) nlst);base case
      ((< (first lst)(get-nth lst 1))(eliminate-largerREC (rest (rest lst)) (cons (first lst) nlst)))
      (else
       (eliminate-largerREC (rest lst) (cons (first lst) nlst)))))



;get-nth DONE
(define (get-nth lst n)
    (cond
      ((empty? lst) -1)
      ((= n 0) (first lst))
      ((> n (length lst)) -1)
      ((> n 0) (get-nth (rest lst)(- n 1)))))


;find-item DONE
(define (find-item lst target);wrapper function for the rucursive one
  (find-itemREC lst target 0))

(define (find-itemREC lst target index)
    (cond
      ((empty? lst) -1);base case
      ((equal? (first lst) target) index)
      (else
       (set! index (+ index 1));add one to the index counter
       (find-itemREC (rest lst) target index))))