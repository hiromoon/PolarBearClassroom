#lang racket
;squrtの実装
(define (square a) (* a a))
(define (cube a) (* a a a))

(define (sqrt n)
  (sqrt-iter 1 n))

(define (sqrt-iter guess x)
    (define (improve)
      (average guess (/ x guess)))

    (define (average x y)
      (/ (+ x y) 2.0))

    (define (good-enough?)
      (< (abs (- (square guess) x)) 0.001))

    (define (sqrt x)
      (sqrt-iter 1.0 x))

  (if (good-enough?)
      guess
      (sqrt-iter (improve) x)))

; practice1.8の実装
(define (curt n)
  (iter-curt 1 n))

(define (iter-curt guess n)
  (define (good-enough?)
          (< (abs (- (cube guess) n)) 0.001))
  (define (improve)
    (/ (+ (/ n (square guess)) (* 2 guess)) 3.0 ))

  (if (good-enough?)
      guess
      (iter-curt (improve) n)))

(display (curt 8))





