# SICP: 1章
## 1.2.4 指数計算
### 指数を求める関数
#### 再帰的な記述方法。
指数の計算方法を再帰と反復で表現する。
```lisp
(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1))
        )
    )
)
(display (expt 2 2))
```
* ステップ数: ø(n)
* 空間: ø(n)

=> 直感的に記述できていい。
　　しかし、相変わらず空間とステップ数が同値。

#### 反復的な記述方法
反復の記述となると下記の通りとなる。
```lisp
(define (expt b n) (expt-iter b n 1))
(define (expt-iter b count product)
    (if (= 0 count) product ;;ここには最終的に返したい値を記述する。
        (expt-iter b (- count 1) (* b product))))

(display (expt 2 2))
```

* ステップ数：ø(n)
* 空間数: ø(1)
処理結果をスタックにためておく必要がないため。

#### 少し工夫して計算量を減らす。
b^4 = b^2 * B^2より、
計算の回数を1/2に減らすことが可能である。
```lisp
(define (fast-expt b n) 
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (even? n)
    (= (remainder n 2) 0))

(display (fast-expt 2 5))
```
=> こいつの反復アルゴリズムを記述するのはとても難しいらしい。

## 1.2.5 最大公約数
ユーグリッド除算法で最大公約数を求めることが可能。
ユーグリッド除算法に関しては、下記を参照
[2.Greatest Common Divider](:note:34e9acc2-8df6-46a5-8b95-a0d88446feb0)

### ユーグリッド除算法の実装
ユーグリッド除算表の実装は簡単にできる。
```lisp
(define (gcd a b)
    (if (= b 0) a
        (gcd b (remainder a b))))

(display (gcd 206 40))
```

### ユーグリッド除算法のステップ数
* ユーグリッド除算法は対数的にステップ数が増加する。
* Lameの定理より⼿続きのn を⼆つの⼊⼒のうちの⼩さいほうだとします。プロセスがk ス
テップかかるとすると、下記が成り立つ。
$$
 k=\log_9{\varphi}n 
$$

## 1.2.6 例：素数判定
整数nの素数性をチェックする２つの方法に関して見ていく。
１つはø(√n)のステップ数のもの。もう一つはø(logn)の確率的なプログラミング

### 約数を探す。
自分自身が最小の約数ならば、それが素数であるという方針からの実装。
```lisp
;; 約数が自分自身かを調べて素数かを判定する。
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divised? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
(define (divised? a b) (= (remainder a b) 0))

(define (prime? n) (= n (smallest-divisor n)))

(display (prime? 2))
```
この計算量は√nとなる。
これは、Nの約数がkだったとした場合、N/kも約数になるが、
どちらも、√N以下になることから。


```lisp
#lang racket
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else        (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (prime? n (- times 1)))
        (else #f)))

(define (square n) (* n n))

(display (prime? 2 100))
```
