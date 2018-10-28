#lang racket
(define (square a) (* a a))
(define (max-square-sum a b c)
  (cond ((or (and (> a b) (> b c)) (and (> b a) (> a c)))
            (+ (square a) (square b)))
        ((or (and (> b c) (> c a)) (and (> c b) (> b a)))
            (+ (square b) (square c)))
        (else
            (+ (square c) (square a)))))

(display (max-square-sum 1 2 3))
(display (max-square-sum 2 1 3))
(display (max-square-sum 3 2 1))
(display (max-square-sum 1 3 2))

