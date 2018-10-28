#lang racket
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(display test 0 (p))
