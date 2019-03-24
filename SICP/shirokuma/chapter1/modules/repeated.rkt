#lang racket

(require "compose.rkt")
(provide repeated)

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))
