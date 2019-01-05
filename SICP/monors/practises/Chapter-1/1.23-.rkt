; 1.21
#lan racket
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divided? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n) (+ n n))
(define (divided? n a) (= (remainder a n) 0))

(display (smallest-divisor 199))
(display "\n")
(display (smallest-divisor 1999))
(display "\n")
(display (smallest-divisor 19999))

; 1.22
#lang racket
(define (runtime) (current-inexact-milliseconds))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divided? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (display "")
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (search-for-prime st end)
  (cond ((> st end) (display "\nsearch is end"))
        ((even? st) (timed-prime-test (+ st 1)) (search-for-prime (+ 2 st) end))
        (else (timed-prime-test st) (search-for-prime (+ 2 st) end))))

(define (square n) (+ n n))
(define (divided? n a) (= (remainder a n) 0))

(search-for-prime 100 1000) 

; 1.23
#lang racket
(define (runtime) (current-inexact-milliseconds))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divided? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (display "")
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (search-for-prime st end)
  (cond ((> st end) (display "\nsearch is end"))
        ((even? st) (timed-prime-test (+ st 1)) (search-for-prime (+ 2 st) end))
        (else (timed-prime-test st) (search-for-prime (+ 2 st) end))))

(define (next n) 
  (if (even? n)
      (+ 1 n)
      (+ 2 n)))

(define (square n) (+ n n))
(define (divided? n a) (= (remainder a n) 0))

(search-for-prime 1000 1100) 

; 1.24
; fermer-methodへ置き換えるだけなのでパス。

; 1.25
; 注釈46にある通り。
; 問題の方式だと、指数計算をしたのちにremainderを求めることになるので、
; とても大きな数に対してあまりを求めることとなる。
; しかし、本文中にある方法だと、計算途中の答えに対して余りを求めるので、
; mに対してとても大きな数になることがない。

; 1.26
; (expmod base (/ exp 2) m)が２回呼ばれているため。
; 呼び出し時、木構造にバイバイになっていく。
;
; 普通の呼び出し。
; 10 -> 5 -> 4 -> 2 -> 0 (logn)
; 
; しかし、手書きをしてしまうと、下記のようになる。
; 10 -> 5 -> 4
;         -> 4
;    -> 5 -> 4
;         -> 4
; この呼び出しは、（logn)続くので、(2^logn)より、(n)となる。


; 1.27
#lang racket
(define (square n) (* n n))

(define (prime? n times)
  (cond ((= times 0) #t)
        ((formet-test n) (prime? n (- times 1)))
        (else #f)))

(define (formet-test n)
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else        (remainder (* base (expmod base (- exp 1) m)) m))))
; 561, 1105, 1729, 2465, 2821, 6601
(display "fermat test")
(newline)
(display (prime? 561 10000))
(display (prime? 1105 10000))
(display (prime? 1729 10000))
(display (prime? 2465 10000))
(display (prime? 2821 10000))
(display (prime? 6601 10000))
(newline)

(define (prime2? n)
  (= (smallest-divider n) n))

(define (smallest-divider n)
  (divider-iter 2 n))

(define (divider-iter test-divider n)
  (define divided? (= (remainder n test-divider) 0))
  
  (cond ((> test-divider n) n)
        (divided? test-divider)
        (else divider-iter (+ 1 test-divider n))))

(display "smallest-divider test")
(newline)
(display (prime2? 561))
(display (prime2? 1105))
(display (prime2? 1729))
(display (prime2? 2465))
(display (prime2? 2821))
(display (prime2? 6601))

;1.28
; a^(n-1) = 1 (modulo n)
#lang racket
(define (square n) (* n n))

(define (prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (prime? n (- times 1)))
        (else #f)))
    
(define (miller-rabin-test n)
  (define (try-it a)
    (expmod a (- n 1) n n))

  (= (try-it (+ 1 (random (- n 1)))) 1))

(define (expmod base exp m n)
  (define (unexpected-one? a)
    (if (and(eq1orn-1? a) (fakeOne? a)) 0 a))
     
  (define (eq1orn-1? a) (or (= 1 a) (= (- n 1) a)))
  (define (fakeOne? a) (= (remainder a n) 1))

  (cond ((= exp 0) 1)
        ((even? exp) (remainder (unexpected-one? (square (expmod base (/ exp 2) m n))) m))
        (else (remainder (* base (expmod base (- exp 1) m n)) m))))

; 561, 1105, 1729, 2465, 2821, 6601
(display (prime? 561 1000))
(display (prime? 1105 1000))
(display (prime? 1729 1000))
(display (prime? 2465 1000))
(display (prime? 2821 1000))
(display (prime? 6691 1000))

; 1.29
#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sympthon f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (Y k) (* (y　k) (if (even? k) 4 2)))
  (define (add-n n) (+ n 1))

  (* (/ h 3.0) (+ (y 0) (sum Y 1 add-n (- n 1)) (y n))))

(define (cube n) (* n n n))

(display (sympthon cube 0 1 100))
(newline)
(display (sympthon cube 0 1 1000))
(newline)
(display "actual result:")
(display (/ 1 4.0))

; 1.30
#lang racket
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))

    (iter a 0))

; test
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube n) (* n n n))

(display (integral cube 0 1 0.0001))

; 1.31
; a
#lang racket
(define (products term a next b)
  (if (> a b)
      1
      (* (term a) (products term (next a) next b))))

(define (j_pi times)
    (define (f n)
        (if (even? n)
            (/ (+ n 2) (+ n 1))
            (/ (+ n 1) (+ n 2))))
  
    (define (add n) (+ n 1.0))

    (* 4 (products f 1 add times)))

(display (j_pi 100000))

;b
#lang racket
(define (products term a next b)
  (define (product-iter n result)
    (if (> n b)
        result
        (product-iter (next n) (* result (term n)))))

  (product-iter a 1))

(define (j2_pi times)
    (define (f n)
      (if (even? n)
          (/ (+ n 2) (+ n 1))
          (/ (+ n 1) (+ n 2))))
    (define (add n)
      (+ n 1.0))

    (* 4 (products f 1 add times)))

(display (j2_pi 100000000))
     
; 1.32
; a
#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (products term a next b)
  (accumulate * a term a next b))

(define (j2_pi times)
(define (f n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
(define (add n)
  (+ n 1.0))

(* 4 (products f 1 add times)))

(display (j2_pi 100000))

; b
#lang racket
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter n result)
    (if (> n b)
        result
        (accumulate-iter (next n) (combiner result (term n)))))

    (accumulate-iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (products term a next b)
  (accumulate * a term a next b))

(define (j2_pi times)
(define (f n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
(define (add n)
  (+ n 1.0))

(* 4 (products f 1 add times)))

(display (j2_pi 100000))

; 1.33
; a
#lang racket
(define (filterd-accumulate combiner filter-term null-value term a next b)
  (if (> a b)
      null-value
      (if (filter-term a)
          (combiner (term a) (filterd-accumulate combiner filter-term null-value term (next a) next b))
          (filterd-accumulate combiner filter-term null-value term (next a) next b))))

(define (sum-square-primes a b)
  (filterd-accumulate + prime? 0 square a (lambda (x) (+ x 1)) b))

(define (prime? n)
    (if (= (smallest-divider n) n) #t #f))

(define (smallest-divider n)
    (define (divider-iter n test-divider)
      (cond ((divided? n test-divider) test-divider)
            ((< n test-divider) n)
            (else (divider-iter n (+ test-divider 1)))))

    (define (divided? a b)
      (= 0 (remainder a b) 0))

    (divider-iter n 2))

(define (square n) (* n n))

(display (sum-square-primes 1 10))

; b
#lang racket
(define (products-all-comprime n) 
  (define (comprime? a)
    (= (gcd a n) 1))

  (filterd-accumulate * comprime? 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (filterd-accumulate combiner filter-term null-value term a next b)
  (if (> a b)
      null-value
      (if (filter-term a)
          (combiner (term a) (filterd-accumulate combiner filter-term null-value term (next a) next b))
          (filterd-accumulate combiner filter-term null-value term (next a) next b))))


(products-all-comprime 10)

; 1.34
#lang racket
(define (f g) (g 2))

; (f f) => (f 2) => (2 2)
; となり、エラーが発生する。 

; 1.35
; Ω ^ 2 = Ω + 1
; -> Ω = 1 + 1 / Ω 
#lang racket
(define torelance 0.00001)
(define (fixed-point f first-point)
  (define (closed-enough? v1 v2)
    (< (abs (- v1 v2)) torelance))
  (define (try guess)
    (let ((next (f guess)))
        (if (closed-enough? guess next)
            next
            (try next))))
  (try first-point))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(golden-ratio)

; 1.36
#lang racket
(define torelance 0.0001)
(define (fixed-point f first-point)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
         torelance))
    (define (try guess)
      (let ((next (f guess)))
        (newline)
        (display next)
        (if (close-enough? guess next)
            next
            (try next))))

    (try first-point))

(define (x-exp-x n)
  (fixed-point (lambda (x) (/ (log n) (log x))) 2))

(x-exp-x 1000)

; 9.965784284662087
;3.004472209841214
;6.279195757507157
;3.759850702401539
;5.215843784925895
;4.182207192401397
;4.8277650983445906
;4.387593384662677
;4.671250085763899
;4.481403616895052
;4.6053657460929
;4.5230849678718865
;4.577114682047341
;4.541382480151454
;4.564903245230833
;4.549372679303342
;4.559606491913287
;4.552853875788271
;4.557305529748263
;4.554369064436181
;4.556305311532999
;4.555028263573554
;4.555870396702851
;4.555315001192079
;4.5556812635433275
;4.555439715736846
;4.555599009998291
;4.555493957531389
;4.555563237292884

; 平均緩和法
(define (x-exp-x-av n)
  (fixed-point
    (lambda (x) 
        (/ (+ x (/ (log n) (log x))) 2))
    2))
(x-exp-x-av 1000)

; 5.9828921423310435
;4.922168721308343
;4.628224318195455
;4.568346513136242
;4.5577305909237005
;4.555909809045131
;4.555599411610624
;4.5555465521473675;


; 1.37
; a
#lang racket
;(define (cont-frac n d k)
;  (define (try i)
;    (if (> i k)
;        (d i)
;        (+ (d (- i 1)) (/ (n i) (try (+ i 1))))))
;  (/ (n 1) (try 2)))

; 下記の描き方の方が綺麗だった。
(define (cont-frac n d k)
  (define (try i)
    (if (> i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (try (+ 1 i))))))
  (try 1))
          
(define (iter-a-to-b f a b)
   (newline)
   (display a)
   (display "->")
   (display (f a))
   
   (if (> a b)
      (and (newline)(display "end"))
      (iter-a-to-b f (+ a 1) b)))

(iter-a-to-b
 (lambda (k) (cont-frac (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k))
 1
 20)

; b
#lang racket
(define (cont-frac n d k)
  (define (try i result)
    (let ((current (+ (d (- i 1)) (/ (n i) result))))
        (if (> 0 i)
            result
            (try (- i 1) current))))
  (/ (n 1) (try k (d k))))

(define (iter-a-to-b f a b)
   (newline)
   (display a)
   (display "->")
   (display (f a))
   
   (if (> a b)
      (and (newline)(display "end"))
      (iter-a-to-b f (+ a 1) b)))

(iter-a-to-b
 (lambda (k) (cont-frac (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k))
 1
 20)

;1.38
; 2 -> 2
; 5 -> 4
; 8 -> 6
; 反復バージョン
#lang racket
(define (cont-frac n d k)
  (define (try i)
    (if (> i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (try (+ 1 i))))))
  (try 1))


(define (num-array n)
  (if (= (remainder n 3) 2)
      (/ (* 2 (+ n 1))  3)
      1))

(define (e n)
  (+ 2 (cont-frac 
            (lambda (k) 1.0)
             num-array
             n)))

(define (test-a-b f a b)
  (newline)
  (display (f a))
  (if (> a b)
      (and (newline) (display "end")) 
      (test-a-b f (+ 1 a) b)))

;(test-a-b num-array 1 10)
(e 10)

; 再帰バージョン
(define (cont-frac n d k)
  (define (try i result)
    (let ((current (+ (d (- i 1)) (/ (n i) result))))
        (if (> 2 i)
            result
            (try (- i 1) current))))
  (/ (n 1) (try k (d k))))

(define (num-array n)
  (if (= (remainder n 3) 2)
      (/ (* 2 (+ n 1))  3)
      1))

(define (e n)
  (+ 2 (cont-frac 
            (lambda (k) 1.0)
             num-array
             n)))

(define (test-a-b f a b)
  (newline)
  (display (f a))
  (if (> a b)
      (and (newline) (display "end")) 
      (test-a-b f (+ 1 a) b)))

;(test-a-b num-array 1 10)
(e 100)

; 1.39
#lang racket
(define (tan-fc x n)
  (cont-frac 
    (lambda (k) (if (= 1 k) x (- (* x x))))
    (lambda (k) (- (* 2.0 k) 1))
    n))

(tan-fc 1 100)
; > 1.557407724654902

; 1.40
#lang racket
(define (cube n)
  (* n n n))

(define (square n)
  (* n n))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define dx 0.001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define torelance 0.0001)

(define (fixed-point f x)
  (define (closed-enough? v1 v2)
    (< (abs(- v1 v2)) torelance))
  
  (let ((next (f x)))
    (if (closed-enough? x next)
        x
        (fixed-point f next))))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newton-method (cubic 1 2 3) 1)


; 1.41
#lang racket
(define (double f)
  (lambda (x) (f (f x))))

; dobule test
(define (inc x)
  (+ x 1))
; expected 3(1 (+ 1 + 1))
;(display ((double inc) 1))
;> 3

(((double (double double)) inc) 5)

; 1.42
#lang racket
(define (compose_ f g)
  (lambda (x) (f (g x))))

(define (inc n) (+ 1 n))
(define (square n) (* n n))

((compose_ square inc) 6)

; 1.43
#lang racket
(define (square n) (* n n))

(define (repeated fn n)
  
  (define (repeated-iter f g i)
    (if (= i 0) g
        (repeated-iter f (lambda (x) (f (g x))) (- i 1))))
  ; (lamdba (x) x)って思っていた以上に強力なことがわかった。
  (repeated-iter fn (lambda (x) x) n))

((repeated square 2) 5)

; 以下答えを見たメモ
; 自分の答えは反復的な考え方をしていた。
; たしかし、再帰的な考え方だとかきのようになる。
; また、抽象化を行なった方が、問題を簡易的に捉えられる。
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

; 1.44
; 平滑化...平滑化とはデータに於ける重要パターンを、ノイズなど、重要性の低いものを除去しながら見つけ出す方法です。平滑化の目的は、値の変化をなだらかにしてデータの傾向をわかりやすくすることです。-> excelのグラフにもこんな処理があった気がする。

; smoothの定義をおこなう。
#lang racket 
(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ 
                (+ (f (+ x dx)) (f x) (f (- x dx)))
                3)))

(define (n-smooth f n)
  (repeated smooth n) f)

; 1.45
#lang racket
(define (average a b) (/ (+ a b) 2))

(define (fixed-point f x)
  (let ((next (f x)))
    (if (closed-enough? x next)
        next
        (fixed-point f next))))

(define (closed-enough? v1 v2)
  (< (abs (- v1 v2)) 0.0001))

(define (average-dump f)
  (lambda (x) (average (f x) x)))

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (pow i n)
  (if (= n 0)
      1.0
      (* i (pow i (- n 1)))))
; 実験
(define (n-root n x n-avg)
  (let ((f ((repeated average-dump n-avg)
            (lambda (y) (/ x (pow y (- n 1)))))))
    (fixed-point f x)))

; average
; (average 1.0 2)
; fixed-point
; (fixed-point cos 1.0)
; average-dump
; ((average-dump (lambda (x) (* x x))) 5)
(define (test-a-b f a b)
  (and (newline) (display a) (display ":") (display (f a)))
  (if (> a b)
      (and (newline) (display "end"))
      (test-a-b f (+ a 1) b)))

(test-a-b
 (lambda (n) (n-root n 100000 3))
 2
 100)
; avg-n: 1
; 2:316.2277660168379
; 3:46.41590792403119

; avg-n: 2
; 2:316.2278448853881
; 3:46.41590048347405
; 4:17.782794100420364
; 5:9.999988956665895
; 6:6.81289670349097
; 7:5.17944223309802

; avg-n: 3
; 2:316.22805974882857
; 3:46.41604483288078
; 4:17.782851270953042
; 5:10.000046314350069
; 6:6.812939862890545
; 7:5.179479663970069
; 8:4.216965034285822
; 9:3.593807940353535
; 10:3.162286838240118
; 11:2.8480499189991506
; 12:2.610132919400945
; 13:2.4244309305410523
; 14:2.2758129102640337
; 15:2.1544780437333184

; avg-nは2^nで切り替える。
(define (n-root n x)
  (define n-avg
    (quotient (log 2 n) 2))
  
  (let ((f ((repeated average-dump n-avg)
            (lambda (y) (/ x (pow y (- n 1)))))))

    (fixed-point f x)))

(n-root 5 2)
