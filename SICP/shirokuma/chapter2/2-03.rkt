#lang racket

; pattern 1
(define (make-rectangle height width)
  (cons height width))
(define (height rectangle) (car rectangle))
(define (width  rectangle) (cdr rectangle))
(define (circumference rectangle)(* 2 (+ (height rectangle) (width rectangle))))
(define (area rectangle)(* (height rectangle) (width rectangle)))

; pattern2

;test

(circumference (make-rectangle 4 5)) ; 18
(area (make-rectangle 4 5)) ; 20


