#lang racket

(define nil '())

(define (fringe tree) 
   (define nil '()) 
   (define (fringe-itr x result)
     (cond ((null? x) result)
           ((not (pair? x)) (cons x result)) 
           (else (fringe-itr (car x)
                               (fringe-itr (cdr x) result))))) 
   (fringe-itr tree nil))
  
; test
(define x (list (list 1 2) (list 3 4)))
(define x2 (list (list 1 2) (list 3 4) 5))
(define x3 (list 1 2 3 4 5))

(fringe x)            ; (1 2 3 4)
(fringe (list x x))   ; (1 2 3 4 1 2 3 4)
(fringe x2)           ; (1 2 3 4 5)
(fringe (list x2 x2)) ; (1 2 3 4 5 1 2 3 4 5)
(fringe x3)           ; (1 2 3 4 5)
(fringe (list x3 x3)) ; (1 2 3 4 5 1 2 3 4 5)