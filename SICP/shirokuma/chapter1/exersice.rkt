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



