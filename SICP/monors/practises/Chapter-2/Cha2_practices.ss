; ************** 2.1 ************
#lang racket
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat n)
  (display (numer n))
  (display "/")
  (display (denom n)))
  
(define (make-rat n d)
  (let ((p_n (abs n))
        (p_d (abs d))
        (g (gcd (abs n) (abs d)))
        (sing (if (> (* n d) 0) 1 -1)))

    (cons (* sing (/ p_n g)) (/ p_d g))))

; plus vs plus
(print-rat (make-rat 3 6))
(newline)
; 1 / 2

; plus vs minus
(print-rat (make-rat 3 -6))
(newline)
; - 1 / 2

; minus vs plus
(print-rat (make-rat -3 6))
(newline)
; - 1 / 2

; minus vs nimus
(print-rat (make-rat -3 -6))
; 1 / 2
        
; ************** 2.2 ************
#lang racket
; define segments
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; define points
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (middle-point segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
  (make-point (/ 2 (+ (x-point start-point) (x-point end-point)))
              (/ 2 (+ (y-point start-point) (y-point end-point))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))

(print-point (middle-point (make-segment (make-point 0 0) (make-point 2 2))))

; ************* 2.3 ************
#lang racket
; define segments
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; define points
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; define rectangle
(define (make-rectangle width length) (cons width length))
(define (width rectangle) (car rectangle))
(define (length rectanble) (cdr rectanble))

; segment-length
(define (segment-length segment)
  (let ((X (- (x-point (start-segment segment)) 
              (x-point (end-segment segment))))
        (Y (- (y-point (start-segment segment))
              (y-point (end-segment segment)))))

  (sqrt (+ (square X) (square Y)))))

(define (square n) (* n n))

; 外周の長さ
(define (periphery rectangle)
  (+ (* 2 (segment-length (width rectangle)))
     (* 2 (segment-length (length rectangle)))))

; 長方形の面積
(define (area rectangle)
  (* (segment-length (width rectangle))
     (segment-length (length rectangle))))

; segmentの定義とrectangleを定義
(define segment-a (make-segment (make-point 0 0) (make-point 0 2)))
(define segment-b (make-segment (make-point 0 2) (make-point 2 2)))
(define my-rectanble (make-rectangle segment-a segment-b))

(display (periphery my-rectanble))
(newline)
(display (area my-rectanble))

; 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; car(cons 2 1)
; -> (lambda (m) (m 2 1) m: (lambda (p q) p)
; -> (lambda (2 1) 2)
; -> 2
(define (cdr z)
  (z (lambda (p q) q)))

; 2.5
; 非負整数
#lang racket
(define (pow b n)
  (if (< n 1)
      1
      (* b (pow b (- n 1)))))

(define (cons a b)
    (* (pow 2 a)
       (pow 3 b)))

(define (car z)
  (define (car-iter n)
    (let ((divided? (= (remainder z (pow 2 n)) 0)))
      (if divided?
          (car-iter (+ n 1))
          (- n 1))))
  (car-iter 1))

(define (car z)
  (define (car-iter n)
    (let ((divided? (= (remainder z (pow 2 n)) 0)))
      (if divided?
          (car-iter (+ n 1))
          (- n 1))))
  (car-iter 1))

(define (cdr z)
  (define (cdr-iter n)
    (let ((divided? (= (remainder z (pow 3 n)) 0)))
      (if divided?
          (cdr-iter (+ n 1))
          (- n 1))))
  (cdr-iter 1))

(car (cons 3 2))
(cdr (cons 3 2))
(car (cons 10 11))
(cdr (cons 10 11))
(car (cons 0 1))
(cdr (cons 0 1))
(car (cons 1 0))
(cdr (cons 1 0))

; 2.6
#lang racket
; one と twoの定義
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (inc n) (+ n 1))

; addの定義
(define (add x y)
  (lambda (fnc)
    (lambda (arg)
      ((x fnc) ((y fnc) arg)))))

(define three (add one two))

((three inc) 0)

; 2.7
; 区分の抽象化の実装
(define (make-interval a b) (cons a b))
(define (upper-bound interval) (car interval))
(define (lower-bound interval) (cdr interval))

; 2.8
; 減算の定義
; x + y - y はxとならなければいけないことを考えると、
; lowe, upper同士の減算になることがわかる。
(define (sup-interval x y)
  (make-interval (- ((lower-bound x) (lower-bound y)))
                 (- ((upper-bound x) (upper-bound y)))))

; 2.9
; 証明系なにでパス

; 2.10
#lang racket

; ********* answer ****************
(define (dev-interval x y)
  (define (>= x y) (not (< x y)))
    (if (<= (* (lower-bound x) (lower-bound y) 0)
        (error "this interval stride 0")
        (mul-interval
           x
           (make-interval (/ 1.0 (lower-bound y))
                          (/ 1.0 (upper-bound y))))))
; *********************************

; ********** 補助関数 **************
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (car interval))
(define (lower-bound interval) (cdr interval))
(define (print-interval memo a)
  (newline)
  (display memo)
  (display "[")
  (display (upper-bound a))
  (display ",")
  (display (lower-bound a))
  (display "]"))

; USAGE
(define a (make-interval 1 2))
(define b (make-interval 3 4))

(define basic (dev-interval a b))

(print-interval "BASEIC" basic)

; 2. 11 ~ 2.16に関してはのちの時間に行う。

; 2.11
; intevalのペア同士の掛け算を行い、Min, Maxを算出しそれからIntervalを作成する。
; interval a, bがあると仮定する。
; その場合、aとbの区間が0を起点にどの状態にあるかで９パターンある。
;
; * 全状態
; 1. 0より大きい
; 2. 0
; 3. 0より小さい
; これら状態に関して、3 * 3 = 9 パターンがあることを問題文では言っているよう。
; 
; * 3回以上の掛け算必要とは？
; `min p1 p2 p3 p4`と最小値を見つけるために複数回掛け算を行なっているのでこのことだと思う。
; これに関して、それぞれの9パターンに関してMin, Maxの求め方を場合分けする。

; * 場合分け
; interval a - interval b (Min: elem_a - elem_b, Max; elem_a - elem_b)
; 1 - 1 (Min: low-low, Max: upper-upper)
; 1 - 2 (Min: low-upper(マイナスになるので）, :w
; 1 - 3 (

; 2.17
(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
        (car items)
        (last-pair next))))

(last-pair (list 23 72 149 34))

; 2.18
#lang racket
; 反復
(define (reverse items)
  (define (reverse-iter item result)
    (let ((next ( cdr item ))
          (make-list ( cons (car item) result )))
      
      (if (null? next)
          make-list
          (reverse-iter (cdr item) make-list))))
  (reverse-iter items (list)))

; 答えを見て少し改修
(define (revers-r items)
  (define (reverse-iter item result)
      (if (null? item)
          result
          (reverse-iter (cdr item) (cons (car item) result ))))
  
  (reverse-iter items (list)))


(reverse (list 23 72 149 34))

; 2.19
#lang racket
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values )) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values ))
            (cc (- amount
                   (first-denomination
                    coin-values ))
                coin-values )))))

(define (first-denomination coin-value)
  (car coin-value))

(define (except-first-denomination coin-value)
  (cdr coin-value))

(define (no-more? coin-value)
  (null? coin-value))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

; 全件検索が行われるので計算結果に影響を与えない。

; 1.20
; filter-function
(define (filter func items)
  (let (( nil '() ))
    (if (null? items)
        nil
        (if (func (car items))
            (cons (car items) (filter func (cdr items)))
            (filter func (cdr items))))))

(define (same-parity . x)
  (if (even? (car x)) 
      (filter even? x)
      (filter odd? x)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; 2.21
(define (square x) (* x x))

(define (square-list-b items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list-b (cdr items)))))

(define (square-list-a items)
  (map (lambda (x) (square x))
       items))

(square-list-a (list 1 2 3 4))
(square-list-b (list 1 2 3 4))

; 2.22
#lang racket
; pattern1 -> cons (square ...) answer)の部分が(square ....) とanswerが逆だから。
; pattern2 -> cons number list でlistの伸長が可能だが、その逆は不可なため。

; 改善案
(define nil '())
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      ; 数値をlist化したものを作成する。
                      (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4))

; 2.23
#lang racket
(define (for-each proc items)
  (let ((next-items (cdr items)))
    (let ((do-next (lambda ()
                   (proc (car items))
                   (for-each proc next-items))))
    
      (if (null? next-items)
          (proc (car items))
          (do-next)))))

(for-each (lambda (x)
                  (newline)
                  (display x))
          (list 57 321 88))

; 2.24
; 1, ●  ->  2, ●  -> 3, 4
; ↓         ↓        ↓  ↓
; 1         2        3  4

; 2.25
