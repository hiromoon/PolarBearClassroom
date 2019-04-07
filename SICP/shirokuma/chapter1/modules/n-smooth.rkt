#lang racket

(require "repeated.rkt")
(provide n-smooth)

(define dx 0.0001)

(define (n-smooth f n)
  (repeated (smooth f) n))
(define (smooth f) 
  (lambda (x) 
    (/ (+ (f (- x dx)) 
          (f x) 
          (f (+ x dx))) 
        3)))