; 1.1
; 上から順に結果だけ書く
10
12
8
3
6
表示結果なし（値3の名前がaになった）
表示結果なし（手続き(a+1)の名前がbになった）
19
#f
4
16
6
16

;1.2
( / (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
#lang sicp

(define (square x) (* x x))
(define (sum-of-square x y) ( + (square x) (square y)))
(define (<= x y) (not (> x y)))
(define (sum-of-greater-square x y z) 
    (cond
        ((and (<= x y) (<= x z)) (sum-of-square y z))
        ((and (<= y x) (<= y z)) (sum-of-square x z))
        ((and (<= z x) (<= z y)) (sum-of-square x y))
    )
)

; テスト
; expect 全部13が出力されること
(sum-of-greater-square 1 2 3)
(sum-of-greater-square 1 3 2)
(sum-of-greater-square 2 1 3)
(sum-of-greater-square 2 3 1)
(sum-of-greater-square 3 1 2)
(sum-of-greater-square 3 2 1)

; expect 全部10が出力されること
(sum-of-greater-square 3 1 1)
(sum-of-greater-square 1 3 1)
(sum-of-greater-square 1 1 3)

; expect 2が出力されること
(sum-of-greater-square 1 1 1)

;1.4
ifの述語は複合式である。
評価モデルにより、複合式の評価結果には演算子を返すことができる。
b > 0の場合、a bには + の演算子が適用され、  b > 0を満たさない場合は、- 演算子が適用される。

;1.5
(define (p) (p)) 
(define (test x y)
    (if (= x 0) 0 y)
)

; 適用順序評価
(test 0 (p))
((if (= 0 0) 0 (p)))
; (p)を評価しようとして、無限呼び出しになる。。というかなった。

; 正規順序評価
(test 0 (p))
((if (= x 0) 0 (p)))
0
; 演算子(p)の評価前にif文が評価され、結果は0となる。

;1.6
; 試しに適当な値を入れてみる。
; guess = 4, x = 9

(sqrt-itr 4 9)
(new-if (good-enough? 4 9, 4(sqrt-itr(improve 4 9) 9)))

new-ifは、一般的評価規則の為、被演算子を先に評価しておく必要がある。
imprope(4, 9)は、値として評価できるが、その後(sqrt x y)を再度評価しないといけない為、無限ループになる。

; こーいう時の為に、特殊評価規則がいるのね。


;1.7
;1.8
;1.9
;1.10

;1.11

; 再帰
これはただ書くだけ。

#lang sicp

(define (f n)
    (if (< n 3) n
        (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))


; 反復
少し考えてわからんかったから答え見た。

(define (ff n)
    (define (iter new old old2 count)
        (if (>= count n) new
            (iter (+ new old old2) new old (+ 1 count))))
    (iter 3 2 1 3))


;1.12
再帰問題でよくみるやつ。
r:行、c:列とすると、

#lang sicp

(define (pascal r c) 
    (cond ((= c 1) 1)
          ((= r c) 1)
          (else (+ (pascal (- r 1) (- c 1))
                   (pascal (- r 1) c)))))

; テスト
; たぶんいけた
(pascal 3 2) 2
(pascal 4 2) 3 
(pascal 4 3) 3

;1.13
証明問題はパス

;1.14


 (define (count-change amount) 
   (cc amount 5)) 
 (define (cc amount kinds-of-coins) 
   (cond ((= amount 0) 1) 
         ((or (< amount 0) (= kinds-of-coins 0)) 0) 
         (else (+ (cc amount 
                      (- kinds-of-coins 1)) 
                  (cc (- amount 
                         (first-denomination kinds-of-coins)) 
                      kinds-of-coins))))) 
 (define (first-denomination kinds-of-coins) 
   (cond ((= kinds-of-coins 1) 50) 
         ((= kinds-of-coins 2) 25) 
         ((= kinds-of-coins 3) 10) 
         ((= kinds-of-coins 4) 5) 
         ((= kinds-of-coins 5) 1))) 

木を書くのはつらかった。。
2番目についてはわからんかったので答えみた。

;1.15

(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))

5回？ 

2番目の問はわからんかったので、答えみたがピンとこない。。

; 1.16

#lang sicp

 (define (fast-expt b n) 
   (define (iter a b n) 
     (cond ((= n 0) a) 
           ((even? n) (iter a (square b) (/ n 2))) 
           (else (iter (* a b) b (- n 1))))) 
   (iter 1 b n)) 
 (define (square x) (* x x)) 

 ; テスト 
(fast-expt 2 0) 
(fast-expt 2 1) 
(fast-expt 2 8) 

; 1.17

#lang sicp

(define (multiply a b)
  (define (double x) (* 2 x))
  (define (halve x) (/ 2 x))
  (cond ((= b 0) 0)
        ((even? b) (double (multiply a (halve b))))
        (else (+ a (multiply a (- b 1))))))

; 1.18

TODO

; 1.19

TODO

; 1.20

; 1.21

#lang sicp

(define (smallest-divisor n) (find-divisor n 2)) (define (find-divisor n test-divisor) 
(cond ((> (square test-divisor) n) n) ((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1))))) 
(define (divides? a b) (= (remainder b a) 0)) 
(define (square x) (* x x))


(smallest-divisor 199)
  -> 199
(smallest-divisor 1999)
  -> 1999
(smallest-divisor 19999)
  -> 7

; 1.22

#lang sicp

(define (timed-prime-test n) (newline)
(display n)
(start-prime-test n (runtime)))
(define (start-prime-test n start-time) (if (prime? n)
(report-prime (- (runtime) start-time)))) (define (report-prime elapsed-time)
(display " *** ") (display elapsed-time))

(define (prime? n)
(= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2)) (define (find-divisor n test-divisor) 
(cond ((> (square test-divisor) n) n) ((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1))))) 
(define (divides? a b) (= (remainder b a) 0)) 
(define (square x) (* x x))


解きかけ





