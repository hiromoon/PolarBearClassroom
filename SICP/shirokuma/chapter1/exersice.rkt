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

とりあえず、書籍のsqrtを組み立ててみる。

(define (sqrt x) (sqrt-iter 1.0 x))
(define (sqrt-iter guess x) 
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))

何個か試してみた。

小さすぎて失敗パターン
(sqrt 0.0001) ; 結果が0.03230844833048122と明らかに違う。

大きすぎて失敗パターン
(sqrt 10000000000000) ; 結果が返ってこない。

差分で判定する手続きを定義する。

#lang racket

(define (sqrt x) (sqrt-iter 1.0 x))
(define (sqrt-iter guess x) 
  (if (good-enough-ver2? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough-ver2? guess x) (< (abs (- (improve guess x) guess)) 0.001))
(define (square x) (* x x))

失敗パターンを試してみる。
(sqrt 0.00001)    ; 実行結果は、0.010120218365353947 となり、0.01に近い数値を出せている。
(sqrt 10000000000000)  ;実行結果は、3162277.6601683795 となり結果が返ってきている。

よって、推測値の差分を取るやり方に変更すれば、今まで計測に失敗していた小・大のsqrtについてうまく働くことがわかる。

;1.8

#lang racket

(define (cbrt x) (cbrt-iter 1.0 x))
(define (cbrt-iter guess x) 
  (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x) x)))
(define (improve guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (good-enough? guess x) (< (abs (- (improve guess x) guess)) 0.001))
(define (square x) (* x x))

; テスト
(cbrt 0.001)
(cbrt 1)
(cbrt 8)
(cbrt 27)
(cbrt 28)
(cbrt 1000)

; 実行結果
0.10001409266436927
1.0
2.000004911675504
3.0000005410641766
3.036589948018755
10.000000145265767

それっぽい値は出てるのでOK？

;1.9

(define (+ a b)
(if (= a 0) b (inc (+ (dec a) b)))) の場合、

(+ 4 5) 
((inc (+ (dec 4) 5)))
((inc (+ 3 5)))
((inc ((inc (+ 2 5)))))
((inc ((inc ((inc (+ 1 5)))))))
((inc ((inc ((inc ((inc (+ 0 5)))))))))
((inc ((inc ((inc ((inc 5))))))))
((inc ((inc ((inc (6)))))))
((inc ((inc (7)))))
((inc (8))
(9)
9

となる。遅延演算による縮約が発生している為、再帰プロセスである。

(define (+ a b)
(if (= a 0) b (+ (dec a) (inc b))))の場合、

(+ 4 5) 
(+ (dec a) (inc b))
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

となる。遅延演算は発生していない。
各ステップ実行の結果は、状態変数のみで管理される為、これは反復プロセスである。

;1.10

(define (A x y) 
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A (- 1 1) (A 1 (- 10 1)))
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
...
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
...
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 1 3)))
...
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 16)
(A 0 ...(15回) 2))
2^15 * 2 = 2^16 = 65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 4)
65536

数学的な定義を出してみる。

(define (f n) (A 0 n))
2n。(f n)の式の本体から明らか。

(define (g n) (A 1 n))
2^nな気がするが。。

(define (h n) (A 2 n))
2^2n ?（勘）

;1.11

再帰

#lang racket

(define (f x) 
  (cond 
    ((< x 3) x)
    (else (+ (f (- x 1)) (f (- x 2)) (f (- x 3))))
  )
)

; テスト
(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

; テスト結果
0
1
2
3
6
11

あってるっぽい。

反復

状態変数として保持しておけば良いのは..

* 現在の反復回数
* n-1, n-2, n-3の3断面を持つ変数の4つ。

(define (f x)
  (f-iter XXX)
)
(define (f-iter n1 n2 n3 count)
  (cond (= count 0) n3)
  ()
)


#lang racket

(define (f x) 
  (cond 
    ((< x 3) x)
    (else (+ (f (- x 1)) (f (- x 2)) (f (- x 3))))
  )
)

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 8)


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

TODO

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
(cond ((> (square test-divisor) n) n)((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1))))) 
(define (divides? a b) (= (remainder b a) 0)) 
(define (square x) (* x x))

; TODO 1.23~1.28

; 1.29

#lang racket

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (* (/ h 3.0)
     (sum term 0 inc n)))
(define (sum term a next b) (if (> a b)
  0
  (+ (term a)
  (sum term (next a) next b))))

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
(define (cube x) (* x x x))

; test
(integral cube 0 1 0.01)
(integral-simpson cube 0 1 100)

; 1.30
#lang racket

;; 再帰
(define (sum term a next b) (if (> a b)
  0
  (+ (term a)
  (sum term (next a) next b))))

;; 反復
(define (sum-itr term a next b) 
  (define (iter a result)
    (if (> a b) 
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; test 
(define (inc x) (+ x 1))
(define (identity x) x)
; 55
(sum identity 0 inc 10) 
; 55
(sum-itr identity 0 inc 10) 

; 1.31

#lang racket

(define (factorial x)
  (product identity 1 inc x))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (inc x) (+ x 1))
(define (identity x) x)

; test
; 1
(factorial 0)
; 1
(factorial 1)
; 120
(factorial 5)

; πの近似
#lang racket

(define (pie n)
  (* (product pie-term 1 inc n) 4.00)
)
(define (pie-term n)
  (if (even? n)
      (/ (+ 2 n) (+ 1 n))
      (/ (+ 1 n) (+ 2 n))))
(define (factorial x)
  (product identity 1 inc x))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (inc x) (+ x 1))
(define (identity x) x)

; 3.1417497057380523
(pie 10000)
; 実行完了に5分くらいかかった
(pie 100000)

; 反復product
#lang racket

(define (factorial x)
  (product-itr identity 1 inc x))
(define (product-itr term a next b)
  (define (itr a result)
    (if (> a b)
      result
      (itr (next a) (* result (term a)))))
  (itr a 1))
(define (inc x) (+ x 1))
(define (identity x) x)

; test

; 120
(factorial 5)

; 1.32

#lang racket

; 再帰
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))
  )
)
; 反復
(define (accumulate-itr combiner null-value term a next b)
  (define (itr a result) 
      (if (> a b)
          result
          (itr (next a) (combiner result (term a)))
      ))
  (itr a null-value))

; Test

; Given
(define (inc x) (+ x 1))
(define (identity x) x)

; When & Then
(accumulate + 0 identity 0 inc 5) ; 15
(accumulate + 0 identity 1 inc 5) ; 15
(accumulate * 1 identity 0 inc 5) ; 0
(accumulate * 1 identity 1 inc 5) ; 120

(accumulate-itr + 0 identity 0 inc 5) ; 15
(accumulate-itr + 0 identity 1 inc 5) ; 15
(accumulate-itr * 1 identity 0 inc 5) ; 0
(accumulate-itr * 1 identity 1 inc 5) ; 120

; メモ
; 余分な括弧でかなりはまった。(define (hoge x)(if (...)))

; 1.33

(define (filtered-accumulate pred combiner null-value term a next b)
  (if (> a b)
    null-value
    (if (pred)
      (combiner (term a)
                (filtered-accumulate pred combiner null-value term (next a) next b)))
      null-value
    )
  )
)

; Test
; Given
define (prime? n)
(= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2)) (define (find-divisor n test-divisor) 
(cond ((> (square test-divisor) n) n)((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1))))) 
(define (divides? a b) (= (remainder b a) 0)) 
(define (square x) (* x x))

; When & Then
(filtered-accumulate prime? + 0 identity 1 inc 10)









