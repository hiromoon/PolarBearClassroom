;practice1-1
;やってみればわかるので省略

;practice1-2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;practice1-3
(define (square x) (* x x))
(define (square-sum x y)
  (+ (square x) (square y)))
(define (max-square-sum a b c)
  (square-sum
    (cond ((> a b) a)
          (else b)
          )
    (cond ((> c b) c)
          (else
            (cond ((> a c) a)
                  (else c))))))

;practice1-4
;演算子を手続きとして扱えるので
;bがマイナスの値の場合には、減算にすることで
;絶対値を加算した結果を返している。

;practice1-5
;適用順序評価の場合には式の結果が必要になるまでは評価されないので
;x=0の条件にマッチしてO(1)で終了する。
;正規順序評価では(p)の定義が再帰しているため、無限ループになって一生終了しない

;ニュートン法
;数値xの平方根の推定値としてyがあるとき、yとx/yの平均を取る
;取った値を次の推定値として計算を繰り返すことで近似値を得られる
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (square x) (* x x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;practice1-6
;正規樹所評価の場合に条件式の評価が遅延評価されないので
;無限ループになる

;practice1-7
;よくわかんなかったのですきっぷ

;practice1-8
(define (cbrt x)
  (cbrt-iter 1.0 x))
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))
(define (cube-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-iter (improve guess x) x)))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

;practice1-9
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
;(+ 4 5) // 9
;(+ 3 5) // 8
;(+ 2 5) // 7
;(+ 1 5) // 6
;(+ 0 5) // 5
;9 線形再帰

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9 反復

;practice1-10
;アッカーマン関数
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
;(A 1 10)
;1024
;(A 2 4)
;65536
;(A 3 3)
;65536

;(A 2 4)
;(A 1 (A 2 3))
;(A 1 (A 1 (A 2 2)))
;(A 1 (A 1 (A 1 (A 2 1)))
;(A 1 (A 1 (A 1 2))
;(A 1 (A 1 (A 0 (A 1 1))))
;(A 1 (A 1 (A 0 2)))
;(A 1 (A 1 4))
;(A 1 (A 0 (A 1 3)))
;(A 1 (A 0 (A 0 (A 1 2))))
;(A 1 (A 0 (A 0 (A 0 (A 1 2)))))
;(A 1 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))
;(A 1 (A 0 (A 0 (A 0 (A 0 2)))))
;(A 1 (A 0 (A 0 (A 0 4))))
;(A 1 (A 0 (A 0 8)))
;(A 1 (A 0 16))
;(A 1 32)
;(A 0 (A 1 32))
;...
;65536

;(define (f n) (A 0 n))
; (f n)は2n
;(define (g n) (A 1 n))
; (g n)は2^n
;(define (h n) (A 2 n))
; (h n)は 2^2^(n-1)らしい...(?)

;practice1-11
;再帰
(define (rec-fun n)
  (if (< n 3)
    n
    (+
      (rec-fun (- n 1))
      (* 2 (rec-fun (- n 2)))
      (* 3 (rec-fun (- n 3))))))
;(rec-fun 0) // 0
;(rec-fun 1) // 1
;(rec-fun 2) // 2
;(rec-fun 3)
;(+ (rec-fun 2) (* 2 (rec-fun 1)) (* 3 (rec-fun 0)))
;(+ 2 (* 2 1) (* 3 0))
;(+ 2 2 0) // 4
;(rec-fun 4)
;(+ (rec-fun 3) (* 2 (rec-fun 2)) (* 3 (rec-fun 1)))
;(+ 4 (* 2 2) (* 3 1))
;(+ 4 4 3) // 11
;(rec-fun 5)
;(+ (rec-fun 4) (* 2 (rec-fun 3)) (* 3 (rec-fun 2)))
;(+ 11 (* 2 4) (* 3 2))
;(+ 11 8 6) // 25
;(rec-fun 6)
;(+ (rec-fun 5) (* 2 (rec-fun 4)) (* 3 (rec-fun 3)))
;(+ 25 (* 2 11) (* 3 4))
;(+ 25 22 12) // 59

;反復
;ここ見たらわかった気がしたけど気のせいだった
;http://yoshiko.hatenablog.jp/entry/2014/06/11/SICP%E8%AA%B2%E9%A1%8C1.11%E3%82%92%E3%82%82%E3%81%86%E3%81%84%E3%81%A1%E3%81%A9%E3%81%A6%E3%81%84%E3%81%AD%E3%81%84%E3%81%AB
;nの回数分だけ値をローテーションしながら計算を反復する
;あとで展開してみる
(define (fun n)
  (fun-iter 0 1 2 n))
(define (fun-iter a b c counter)
  (if (= counter 0)
    a
    (fun-iter b c (+ c (* 2 b) (* 3 a)) (- counter 1))))

;practice1-12
(define (pascal x y)
  (cond ((= x 1) 1)
        ((= x y) 1)
        (else
          (+
            (pascal (- x 1) (- y 1))
            (pascal x (- y 1))))))

;practice1-13
;このあたり見たけど数学がわかりませんでした。
;https://sicp-solutions.readthedocs.io/en/latest/docs/problem-1-13.html

;practice1-14
;回答はDraw.ioで
;https://drive.google.com/file/d/1PuPiIzdK_pwbdNxZE8yUtq2EGQN5Md7W/view?usp=sharing
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;practice1-15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;a. 5回
;(sine 12.15)
;(p (sine (/ 12.15 3.0)))
;(p (sine 4.05))
;(p (p (sine (/ 4.05 3.0))))
;(p (p (sine 1.35)))
;(p (p (p (sine (/ 1.35 3.0)))))
;(p (p (p (sine 0.45))))
;(p (p (p (p (sine (/ 0.45 3.0))))))
;(p (p (p (p (sine 0.15)))))
;(p (p (p (p (p (sine (/ 0.15 3.0)))))))
;(p (p (p (p (p (sine 0.05))))))
;(p (p (p (p (p 0.05)))))

;b. やっぱり数学がわかりませんでした

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

;practice1-16
; (+ n 1)がなんかHack感ある。。。
(define (fast-loop-expt b n)
  (fast-loop-expt-iter b (+ n 1) 1))
(define (fast-loop-expt-iter b n a)
  (cond ((= n 0) a)
        ((= n 1) a)
        ((even? n) (fast-loop-expt-iter (square b) (/ n 2) (* a b)))
        (else (fast-loop-expt-iter b (- n 1) (* a b)))))

;practice1-17
(define (halve n)
  (/ n 2))
(define (double n)
  (* n 2))
(define (mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (mul a (halve b))))
        (else (+ (mul a (- b 1)) a))))

;practice1-18
(define (fast-mul a b)
  (fast-mul-iter a (+ b 1) 0))
(define (fast-mul-iter a b acc)
  (cond ((= b 0) acc)
        ((= b 1) acc)
        ((even? b) (fast-mul-iter (double a) (halve b) (+ acc a)))
        (else (fast-mul-iter a (- b 1) (+ acc a)))))

;practice1-19

;practice1-20
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
;正規順序評価
; 4回
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd 40 6)
;(gcd 6 (remainder 40 6))
;(gcd 6 4)
;(gcd 4 (remainder 6 4))
;(gcd 4 2)
;(gcd 2 (remainder 4 2))
;(gcd 2 0) => 2

;適用順序評価
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;...


;practice1-21
(define (smallest-divisor n) (find-divisor n 2)) (define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

;199   -> 199
;1999  -> 1999
;19999 -> 7

;practice1-22
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
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
  
(define (square n) (* n n))

(define (search-for-primes n max)
  (cond ((< max n) (display "finish"))
        (else (timed-prime-test n) (search-for-primes (+ n 2) max))
;1009,   1013,   1019
;10007,  10009,  10037
;100003, 100019, 100043

;practice1-23
(define (timed-prime-test n) (newline)
(display n)
(start-prime-test n (runtime)))
(define (start-prime-test n start-time) (if (prime? n)
(report-prime (- (runtime) start-time)))) (define (report-prime elapsed-time)
(display " *** ") (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
  
(define (square n) (* n n))

(define (search-for-primes n max)
  (cond ((< max n) (display "finish"))
        (else (timed-prime-test n) (search-for-primes (+ n 2) max))
      ))
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

;practice1-24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n) (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000) (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1009)   ;1009   *** 15713 *** 406
(timed-prime-test 1013)   ;1013   *** 16156 *** 4
(timed-prime-test 1019)   ;1019   *** 18369 *** 3
(timed-prime-test 10007)  ;10007  *** 23090 *** 8
(timed-prime-test 10009)  ;10009  *** 20068 *** 8
(timed-prime-test 10037)  ;10037  *** 20398 *** 8
(timed-prime-test 100043) ;100043 *** 24081 *** 24
(timed-prime-test 100003) ;100003 *** 28211 *** 25
(timed-prime-test 100019) ;100019 *** 24688 *** 24

;practice1-25
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;1009   *** 3494
;1013   *** 170
;1019   *** 163
;10007  *** 49393
;10009  *** 5770
;10037  *** 5286
;100003 *** 321562
;100019 *** 258125
;100043 *** 272366

;practice1-26
;偶数の場合にexpmodが2回呼ばれるため
;expmod(4/2) -> 2 -> expmod(2/2) -> 1 -> expmod(1) => 3回 
;expmod(4/2)expmod(4/2) -> 2 2 -> expmod(2/2)expmod(2/2) expmod(2/2)expmod(2/2) -> expmod(1) .. => 10回

;practice1-27
;全部falseになった
;[カーマイケル数](https://ja.wikipedia.org/wiki/%E3%82%AB%E3%83%BC%E3%83%9E%E3%82%A4%E3%82%B1%E3%83%AB%E6%95%B0)
(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)
(fermat-test 8911)

;practice1-28
;回答動いたけどよくわからん
(define (miller-rabin-test n) 
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1))
    (check-it (expmod a (- n 1) n))))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (define (square-with-check x)
    (define (check-nontrivial-sqrt1 n square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
        0
        square))
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (square-with-check (expmod base (/ exp 2) m)))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))



(miller-rabin-test 1009)   
(miller-rabin-test 1013)   
(miller-rabin-test 1019)   
(miller-rabin-test 10007)  
(miller-rabin-test 10009)  
(miller-rabin-test 10037)  
(miller-rabin-test 100043) 
(miller-rabin-test 100003) 
(miller-rabin-test 100019) 

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)
(miller-rabin-test 8911)

;practice1-29
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (a > b)
    0
    (+ (term a)
       (sum term (next a) b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (inc n) (+ n 1))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y a k)
    (+ a (* h k)))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k) 
    (* (coef k) (f (y a k))))
  (* (/ h 3)
     (sum term 0 inc n)))

(integral cube 0 1 0.01)  ;0.24998750000000042
(integral cube 0 1 0.001) ;0.249999875000001
(simpson cube 0 1 100)    ;0.24999999999999992
(simpson cube 0 1 1000)   ;0.2500000000000002

;practice1-30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum 1 cube inc 10) ;3025

;practice1-31
;a 線形再帰
(define (product term a next b)
  (if (> a b)
       1
       (* (term a)
          (product term (next a) b))))
;b 反復
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

;factorial
(define (factorial n) 
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (product identity 1 inc n))

(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

;pi
(define (pi k) 
  ;分母 x_i = 3 + 2(i / 2)
  (define (numerator i)
        (+ 3.0 (* (quotient i 2.0) 2)))
  ;分子
  (define (denominator i)
        (+ 2.0 (* 2 (quotient (+ i 1) 2.0))))
  (* 4 (/ (product denominator 0 inc k)
     (product numerator 0 inc k))))

(pi 10) ;3.023170192001361

;practice1-32
;a 再帰プロセス
(define (accumulate combiner null-value term a next b) 
  (if (> a b)
    null-value
    (combiner
      a
      (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (define (combiner a acc) (+ acc a))
  (accumulate combiner 0 term a next b))

(define (product term a next b)
  (define (combiner a acc) (* acc a))
  (accumulate combiner 1 term a next b))

(define (identity x) x)
(define (inc n) (+ n 1))

(sum identity 1 inc 10)
(product identity 1 inc 10)

;b 線形プロセス
(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (combiner a acc))))
  (iter a null-value))

(define (sum term a next b)
  (define (combiner a acc) (+ acc a))
  (accumulate combiner 0 term a next b))

(define (product term a next b)
  (define (combiner a acc) (* acc a))
  (accumulate combiner 1 term a next b))

(define (identity x) x)
(define (inc n) (+ n 1))

(sum identity 1 inc 10)
(product identity 1 inc 10)

;practice1-33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (if (filter a)
        (iter (next a) (combiner a acc))
        (iter (next a) acc)))
  (iter a null-value))

;a 素数の2乗和
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
  
(define (square n) (* n n))

(define (square-prime-sum)
  (define (combiner a acc) (+ a acc))
  (filtered-accumulate prime? combiner 0 identity a inc b))

;b GCD(i, n) = 1
(define (product-gcd a b)
  (define (filter a) (= (gcd a b) 1))
  (define (combiner a acc) (* a acc))
  (filtered-accumulate filter combiner 1 identity a inc b))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
