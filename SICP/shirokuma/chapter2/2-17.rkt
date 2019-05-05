#lang racket

(define (my-last-pair list)
  (if (null? (cdr list))
      (car list)
      (my-last-pair (cdr list))))

; test
(define listA (list 23 72 149 34))
(define listB (list 23 34))
(define listC (list 34))

(my-last-pair listA)  ; 34
(my-last-pair listB)  ; 34
(my-last-pair listC)  ; 34