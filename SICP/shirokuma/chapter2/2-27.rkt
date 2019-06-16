#lang racket

(define nil '())

(define (deep-reverse list)
  (define (reverse-itr list result)
    (if (null? list)
        result
        (reverse-itr (cdr list) (cons (reverse(car list)) result))))
  (reverse-itr list nil))

(define (reverse list)
  (define (reverse-itr list result)
    (if (null? list)
        result
        (reverse-itr (cdr list) (cons (car list) result))))
  (reverse-itr list nil))

; test
(define x (list (list 1 2) (list 3 4)))
x                  ; ((1 2) (3 4))
(reverse x)        ; ((3 4) (1 2))
(deep-reverse x)   ; ((4 3) (2 1))