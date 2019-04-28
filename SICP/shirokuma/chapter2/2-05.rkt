#lang racket

(define (exp base n) 
   (define (iter x result) 
     (if (= 0 x) 
         result 
         (iter (- x 1) (* base result)))) 
   (iter n 1)) 

 (define (count-0-remainder-divisions n divisor) 
   (define (iter try-exp) 
     (if (= 0 (remainder n (exp divisor try-exp))) 
         (iter (+ try-exp 1))
         (- try-exp 1))) 
   (iter 1)) 

; Answer
 
;; cons, car, cdr 
(define (my-cons a b) (* (exp 2 a) (exp 3 b))) 
(define (my-car z) (count-0-remainder-divisions z 2)) 
(define (my-cdr z) (count-0-remainder-divisions z 3)) 
  
;; test

(my-cons 3 4) ; 2^3*3^4 = 8*81 = 648
(define test (my-cons 3 4))
(my-car test) ; 3
(my-cdr test) ; 4