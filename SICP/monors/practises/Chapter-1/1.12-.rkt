#lang racket

; 1.12
(define (paskal d w)
  (pas-iter d w d))
  
(define (pas-iter d w n)
  (define end? (or (= w 1) (= w n) (= d 1)))
  (if end?
      1
      (+ (pas-iter (- d 1) (- w 1) n) (pas-iter (- d 1) w n))))

(display (paskal 4 2))

; 1.13
; 証明問題なのでパス

; 1.14
; 空間:n^k (木として分岐していくので。)
; ステップ数：n

; 1.15
; 別紙回答

; 1.16
; 不変量: ab^n
; 偶数:(b^2)^n/2
; 奇数:b^n-1 * b
#lang racket
(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))
(define (fast-expt b n)
  (expt-iter b n 1))
(define (expt-iter b n a)

  (define expt-calc
    (if (even? n)
        (fast-expt (square b) (/ n 2) a)
        (fast-expt b (- n 1) (* a b)))) 
  
  (if (< n 0)
      a
      expt-calc))

(display (expt 2 4))

; 1.17
; 戦略
; 2b = b + b
; 4b = 2b + 2b
; b * n = b * (n / 2) * 2 ; if even
; b * n = b * (n - 1) + b; if not even
; アルゴリズムの書き出しを行えばすぐに解けた。
#lang racket

(define (fast-mul b n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-mul b (halve n))))
        (else (+ b (fast-mul b (- n 1))))))

(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

(display (fast-mul 2 3))

; 1.18
; 戦略
; 不変式：a + b * n 
; 偶数: 2b * (n / 2)
; 奇数: b * (n - 1) + b
#lang racket

(define (fast-mul b n)
  (mul-iter b n 0))

(define (mul-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (mul-iter (double b) (halve n) a))
        (else (mul-iter b (- n 1) (+ a b)))))

(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even a) (= (remainder a 2) 0))

(display (fast-mul 2 3))

; 1.19
#lang racket
; http://www.serendip.ws/archives/381
; 解けなかった。そのまま計算すればよかった。
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0 ) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p p ) (* q q))
                                 (+ (* 2 p q) (* q q))
                                 (/ count 2)))
        (else fib-iter (+ (* a q) (* b q) (* a p))
                       (+ (* a q) (* b p))   
                       p
                       q
                       (- count 1))))
