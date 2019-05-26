#lang racket

(define nil '())

; case1
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list1 (cdr items)))))

; case2
(define (square-list2 items)
  (map square items))
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (square x) (* x x))

; test
(square-list1 (list 1 2 3 4)) ; (1 4 9 16)
(square-list1 (list 5)) ; (25) 
(square-list2 (list 1 2 3 4)) ; (1 4 9 16)
(square-list2 (list 5)) ; (25)