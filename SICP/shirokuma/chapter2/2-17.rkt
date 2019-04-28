#lang racket

(define (last-pair items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) items)))
  (iter items items))

; test
(last-pair (list 1))      ;1
(last-pair (list 1 2 3))  ;3