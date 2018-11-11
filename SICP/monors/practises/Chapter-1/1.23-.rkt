; 1.21
#lang racket
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
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

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
