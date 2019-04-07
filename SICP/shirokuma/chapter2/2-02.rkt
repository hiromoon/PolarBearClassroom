#lang racket

(define (mid-point-seqment p1 p2)
  (make-seqment (/ (+ (start-seqment p1) (start-seqment p2)) 2)
                (/ (+ (end-seqment p1) (end-seqment p2)) 2)))

(define (make-seqment start end) (cons start end))
(define (start-seqment seqment) (car seqment))
(define (end-seqment   seqment) (cdr seqment))

(define (print-point p)
  (newline)
  (display "(")
  (display (start-seqment p))
  (display ",")
  (display (end-seqment p))
  (display ")"))

; test
(define p1 (make-seqment 1 2))
(define p2 (make-seqment 3 4))
(print-point (mid-point-seqment p1 p2)) ; (2, 3)