#lang racket
; 戦略
; 2b = b + b
; 4b = 2b + 2b
; b * n = b * (n / 2) * 2 ; if even
; b * n = b * (n - 1) + b; if not even
; アルゴリズムの書き出しを行えばすぐに解けた。
(define (fast-mul b n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-mul b (halve n))))
        (else (+ b (fast-mul b (- n 1))))))

(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

(display (fast-mul 2 3))


