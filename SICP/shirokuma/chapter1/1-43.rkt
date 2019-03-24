#lang racket

(require "modules/repeated.rkt")

(define (square x) (* x x))
((repeated square 1) 5) ; expect  25 ((square(5))^1)
((repeated square 2) 5) ; expect 625 ((square(5))^2)

; 一応満たしたが、いまいち不安。