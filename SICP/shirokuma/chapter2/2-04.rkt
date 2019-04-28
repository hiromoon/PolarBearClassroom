#lang racket

; Given
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; Answer

; cdr
(define (cdr z)
  (z (lambda (p q) q))) 

; Evaluation
(car (cons x y)) 
(car (lambda (m) (m x y))) 
((lambda (m) (m x y)) (lambda (p q) p)) 
((lambda (p q) p) x y) 
x 
