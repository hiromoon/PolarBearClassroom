#lang racket

(define nil '())

(define (reverse list)
  (define (reverse-itr list result)
    (if (null? list)
        result
        (reverse-itr (cdr list) (cons (car list) result))))
  (reverse-itr list nil))

; test
(define listA (list 1 4 9 16 25))
(define listB (list 25 16 9 4 1))
(define listC (list 1))

(reverse listA)  ; 25 16 9 4 1
(reverse listB)  ; 1 4 9 16 25
(reverse listC)  ; 1