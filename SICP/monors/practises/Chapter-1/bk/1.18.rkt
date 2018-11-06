#lang racket
; 反復
; これは1.16と同じ考え方なのですぐに解けた
(define (fast-mul a b) (mul-iter a b 0))

(define (mul-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (mul-iter (double a)
                             (halve b)
                             product))
        (else (mul-iter a
                        (- b 1)
                        (+ a product)))))

(define (double a) (+ a a ))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

(display (fast-mul 2 10))
