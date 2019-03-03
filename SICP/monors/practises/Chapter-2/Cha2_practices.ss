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
#lang racket
(define a '(1 3 ( 5 7) 9))
(cadr (cadr (cdr a)))

(define b '((7)))
(car (car b))

(define c '(1 (2 (3 (4 (5 (6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr c))))))

; 2.26
'(1 2 3 4 5 6 )
'((1 2 3) 4 5 6)
'((1 2 3) (4 5 6))

; 2.27
#lang racket
(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (if (pair? (car items))
                                    (deep-reverse (car items))
                                    (car items))
                                result))))
  
  (iter items '()))

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 (list 3 4)))
(define z (list (list 1 2) 3 2))
(define zero '())
(define one '(1))

(reverse x)
(deep-reverse x)

(reverse y)
(deep-reverse y)

(reverse z)
(deep-reverse z)

(reverse zero)
(deep-reverse zero)

(reverse one)
(deep-reverse one)

;2.28
(define (fringe items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result (if (pair? (car items))
                          (fringe (car items))
                          (list (car items)))))))
  (iter items '()))

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 (list 3 4)))
(define z (list (list 1 2) 3 2))
(define zero '())
(define one '(1))

; 2.29
; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; test 
; defined modile
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define branch-a (make-branch 10 10))
(define branch-b (make-branch 20 20))
(define mobile-children (make-mobile branch-b branch-a))

(define branch-c (make-branch 10 30))
(define branch-d (make-branch 20 mobile-children))

(define mobile-parent (make-mobile branch-c branch-d))

(left-branch mobile-parent)
(right-branch mobile-parent)

(branch-length branch-d)
(branch-structure branch-d)

; b
(define (total-weight mobile)
  (define (get-branch-weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  
  (if (not (pair? mobile))
      mobile
      (+ (get-branch-weight (right-branch mobile))
         (get-branch-weight (left-branch mobile)))))

; c
; right = left
; nullの場合; #tを返す。

;    * (left, right)
; |--------|
; *        *
;(10, 50) (2, 25)

(define (balanced? mobile)
  (define (togle branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
        
  (if (not(pair? mobile))
      #t
      (and (balanced? (branch-structure (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           ; 自身の評価
           (= (togle (right-branch mobile))
              (togle (left-branch mobile))))))

; test
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(define d (make-mobile (make-branch 10 a) (make-branch 12 5)))
 ;; Looks like: ((10 ((2 3) (2 3))) (12 5)) 

(total-weight d) ;; 11
(balanced? d) ;; #t 

; d
; 下記の通り変更れば、対応可能.
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; 2.30
(define (square n) (* n n))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
;test
(define (square-tree 
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))

; 2.31
#lang racket
(define (square n) (* n n))

(define (tree-map fanc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fanc sub-tree)
             (fanc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;test
(square-tree 
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))



; 2.32
; (1, 2, 3)
; (() (1) (2) (3) (1 2) (1 3)
;１が選ばれた場合、選ばれなかった場合
#lang racket
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest ;carの数が選ばれなかった場合
                (map (lambda (elems)
                       (if (not (pair? elems))
                           (list (car s))
                           (append (list (car s)) elems)))
                     rest))))) ; carの数が選ばれた場合。

(subsets '(1 2 3))
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; yはaccumulaterで処理が行われた結果が返ってくるので、それを時処理とどう組み合わせるかを考えればよい。

(define (map p sequence)
  (accumulate
    (lambda (x y)
      (cons (p x) y))
    '()
    sequence))
; test
(map (lambda (x)
       (+ x 1))
     (list 1 2 3))
; -> '(2 4 7)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; test
(append (list 1 2 3) (list 4 5 6))
;-> '(1 2 3 4 5 6)

(define (length sequence)
  (accumulate
   (lambda (x y)
     (+ 1 y))
   0
   sequence))
   
  
;test
(length (list 1 2 3 4))

; 2.34
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate 
    (lambda (this-coeff higher-terms)
      (+ (* x higher-terms) this-coeff))
    0
    coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
; 79

; 3.35
#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond (( null? tree) '())
        ((not (pair? tree )) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree ))))))

(define (count-leaves t)
  (accumulate 
    +
    0
    (map (lambda (x) 1) (enumerate-tree t))))

; test
(count-leaves (list (list 1 2) (list 3 5 4)))
; -> 5

(count-leaves (list (list 1 2) (list 1 2) (list 3 5 4)))
; -> 7

; 3.36
; 戦略
;  ;seqの一番上を処理する。
;seqの一番上を飛ばした要素を渡す。
#lang racket
(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

       
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             '()
             (cons (accumulate
                      op
                      init
                      (map (lambda (line) (car line)) seqs))
                    (accumulate-n
                      op
                      init
                      (map (lambda (line) (cdr line)) seqs)))))

(define x '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(accumulate-n + 0 x)
; -> '(22 26 30)

; 3.37
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 ;; accumulate-n 
 (define (accumulate-n op init sequence) 
   (define nil '()) 
   (if (null? (car sequence)) 
       nil 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence))))) 
  
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

 ;; Test 
 (dot-product (list 1 2 3) (list 4 5 6)) 

(define (matrix-*-vector m v)
  (map
    (lambda (row)
      (dot-product v row))
    m))

 (define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 

;; Test
(matrix-*-vector matrix (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n 
    cons
    '()
    mat))

;; Test
(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
              (map 
                (lambda (row)
                  (matrix-*-vector cols row))
                m)))

(define (matrix-*-matrix_2 m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-row)
                  (dot-product m-row n-row))
                cols))
    m)))

(define matrix_2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; Test
(matrix-*-matrix matrix matrix_2)
; -> ((70 80 90) (158 184 210) (246 288 330))

(matrix-*-matrix_2 matrix matrix_2)
; -> ((70 80 90) (158 184 210) (246 288 330))

(dot-product (list 1 2 3 4) (list 1 4 7 10))

; 2.38
#lang racket
; 一番右まで行かないと評価が始まらない。
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; 左から次々に評価が行われる。
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

; opが満たさないといけない条件
; 右と左のどちらから計算しても結果が同じになる演算子である必要がある。

; 2.39
#lang racket
; 一番右まで行かないと評価が始まらない。
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; 左から次々に評価が行われる。
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse_r sequence)
  (fold-right
    (lambda (x y)
      (append y (list x)))
    '()
    sequence))

(define (reverse_l sequence)
  (fold-left
   (lambda (x y)
     (cons y x))
     '()
     sequence))

(reverse_r (list 1 2 3 4))

(reverse_l (list 1 2 3 4))

; 3.40
#lang racket
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1 ) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap func sequence)
  (accumulate append '() (map func sequence)))

;; unique-pairs
(define (unique-pairs n)
  (flatmap 
    (lambda (i)
      (map (lambda (j)
             (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

;; test
(enumerate-interval 1 5)
; '(1 2 3 4 5)
(unique-pairs 5)
; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))


;; helper functions
(define (make-pair-sum sequence)
  (list (car sequence) (cadr sequence) (+ (car sequence) (cadr sequence))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (square n) (* n n))
(define (divided? n a) (= (remainder a n) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divided? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= (smallest-divisor n) n))

;; rewrite 'prime-sum-pairs'
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; 2.41
#lang racket
;; helper function
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap func sequence)
  (accumulate append '() (map func sequence)))

;; 列挙
(define (enumerate-interval-ordered-trio n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 (- j 1))))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; filter
(define (sum-is n)
  (lambda (xs) (= n (sum xs))))

(define (sum list)
  (accumulate
   +
   0
   list))

;; build function
(define (find-sum-is s max-num)
  (filter (sum-is s)
          (enumerate-interval-ordered-trio max-num)))

; (enumerate-interval-ordered-trio 6)
(find-sum-is 10 5)
                
; 2.42
#lang racket
;;        2
;;     3  ^  4
;;       \|/
;;   1 <- ● ->
;;       /|\
;;        V
;; (a, b)の座標にk番目のQueenを配置する。
;; 1. x = aの座標はすべて除外
;; 2. y = bの座標はすべて除外
;; 3. y - b = x - a <=> + y + a - ( b + x )  = 0 の座標はすべて除外
;; 4. y - b = - ( x - a ) <=>  + x + y - ( b + a ) = 0の座標はすべて除外
(define (safe? k positions)
  (define (iter n pos)
    (let ((A (car (car positions)))
          (B (cadr (car positions))))
      (if (null? pos)
          #t
          (let ((X (car (car pos)))
                 (Y (cadr (car pos))))
             
             (cond ((= A X) #f)
                   ((= B Y) #f)
                   ((= 0 (- (+ Y A) (+ X B))) #f)
                   ((= 0 (- (+ X Y) (+ A B))) #f)
                   (else (iter (- n 1) (cdr pos))))))))
  
    (iter (- k 1) (cdr positions)))

;; test
; (safe? 4 (list (list 3 4) (list 2 1) (list 1 8)))

(define empty-board '())

;; new queen positon add to rest-of-queens
;; rest-of-queens: (4 1) (3 4) (2 1) (1 7)
;; k(next-queen): 5
;; new-row: 1~8
(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

;;test
; (define rest-of-queens '('(4 2) '(3 4) '(2 5) '(1 2)))
; (adjoin-position 1 5 queens)


;;;;;;;;;helper functions;;;;;;;;;;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap func sequence)
  (accumulate append '() (map func sequence)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;; anser ;;;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions ))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens ))
                 (enumerate-interval 1 board-size )))
          (queen-cols (- k 1))))))
  (queen-cols board-size ))

(queens 10)

; 練習問題 2.43
(flatmap
  (lambda (new-row)
    (map (lambda (rest-of_queens)
           (adfoin-positon new-row k rest-of-queens))
         (queen-cols (- k 1))))
    (enumerate-interval 1 board-size))

; 逆にするとなぜプログラムが遅くなるのか?
; -> queen-colsが何回再帰的に呼ばれるかで処理じかんに差異が出てくる。
; Louisの記述したプログラムの場合、lambda内にqueen-colsがあり、lambdaが実行されるごとに呼び出しが行われる。しかし、2.42の例はflatmapの呼び出しで呼ばれるの呼び出し回数が少なくなる。

; 処理時間に関して
; ８クイーンパズルを解く時間をTとした場合、逆にした場合は8T
         
; 2.44
#lang sicp
(#%require sicp-pict)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let (( top-left (beside up up))
              (bottom-right (below right right ))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner ))))))

(define (right-split painter n)
  (if (= n 0)

      painter
      (let (( smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller )))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (corner-split einstein 5))

; 2.45
#lang sicp
(#%require sicp-pict)

(define right-split (split beside below))
(define up-split (split below beside))

(define (split first second)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (first painter (second smaller smaller))))))

(paint (right-split einstein))
(paint (up-split einstein))

; 2.46
#lang sicp
(#%require sicp-pict)

;; 自分の回答
(define (split first second)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (first painter (second smaller smaller)))))

  (lambda (painter n) (iter painter n)))

;; 下記の様に記述する事も可能。
(define (split_2 origin-placer split-placer)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let (( smaller ((split_2 origin-placer split-placer) painter (- n 1)))) ;; split_2はlambdaのラッパーなのでこれでもOK
          (origin-placer painter (split-placer smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 5))
(paint (up-split einstein 5))

; 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

(define (add-vect v1 v2) 
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2) 
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect)) (* s (ycor-vect))))

; 2.47
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

; 2.48, 49
; painterの使い方
#lang racket
(require sicp-pict)

;; segment
(define seg-list (list (make-segment (make-vect 0 1) (make-vect 1 0))))

;; for making-frame
(define origin (make-vect 0 0))
(define edge1 (make-vect 0 1))
(define edge2 (make-vect 1 0))
(define frame (make-frame origin edge1 edge2))

(paint (segments->painter seg-list))

;; answer
#lang racket
(require sicp-pict)

;; 2.49
;; a
(define seg-list_a
  (list (make-segment (make-vect 0 0) (make-vect 1 0))
        (make-segment (make-vect 1 0) (make-vect 1 1))
        (make-segment (make-vect 1 1) (make-vect 0 1))
        (make-segment (make-vect 0 1) (make-vect 0 0))
        ))

(paint (segments->painter seg-list_a))

;; b
(define seg-list_b
  (list (make-segment (make-vect 0 0) (make-vect 1 1))
        (make-segment (make-vect 1 0) (make-vect 0 1))
        ))

(paint (segments->painter seg-list_b))

;; c
(define seg-list_c
  (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
        (make-segment (make-vect 0.5 0) (make-vect 1. 0.5))
        (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
        (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
        ))

(paint (segments->painter seg-list_c))

;; waveの記述はきついのでパス。

; 2.50
#lang racket
(require sicp-pict)

 (define (flip-horiz painter) 
   (transform-painter painter 
                      (make-vect 1.0 0.0) 
                      (make-vect 0.0 0.0) 
                      (make-vect 1.0 1.0))) 
  
 (define (rotate180 painter) 
   (transform-painter painter 
                      (make-vect 1.0 1.0) 
                      (make-vect 0.0 1.0) 
                      (make-vect 1.0 0.0))) 
  
 (define (rotate270 painter) 
   (transform-painter painter 
                      (make-vect 0.0 1.0) 
                      (make-vect 0.0 0.0) 
                      (make-vect 1.0 1.0))) 

(display "original\n")
(paint einstein)

(display "flip-horize\n")
(paint (flip-horize einstein))


; 2.51
#lang racket
(require sicp-pict)

(define (beside painter1 painter2)
  (let (( split-point (make-vect 0.0 0.5)))
    (let (( paint-bottom
            (transform-painter
             painter1
             (make-vect 0.0 0.0)
             (make-vect 1.0 0.0)
             split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame )))))

(paint (beside einstein einstein))


#lang racket
(require sicp-pict)

(define (beside painter1 painter2)
  (let (( split-point (make-vect 0.5 0.0)))
    (let (( paint-left
            (transform-painter
             painter1
             (make-vect 0.0 0.0)
             split-point
             (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame )))))

 (define (rotate180 painter) 
   (transform-painter painter 
                      (make-vect 1.0 1.0) 
                      (make-vect 0.0 1.0) 
                      (make-vect 1.0 0.0))) 
  
 (define (rotate270 painter) 
   (transform-painter painter 
                      (make-vect 0.0 1.0) 
                      (make-vect 0.0 0.0) 
                      (make-vect 1.0 1.0))) 



 (define (below-2 painter1 painter2) 
   (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

; 2.52 
#lang racket
(require sicp-pict)

;; a
(define wave 
  (segments->painter (list 
                      (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                      (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                      (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                      (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                      (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                      (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                      (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                      (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                      (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                      (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                      (make-segment (make-vect .4 1) (make-vect .6 1)) 
                      (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                      (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                      (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                      (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                      (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                      (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                      (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                      (make-segment (make-vect .75 0) (make-vect .6 0)) 
                      (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                      (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                      (make-segment (make-vect .4 0) (make-vect .25 0))
                      ;; add some line
                      (make-segment (make-vect .4 .9) (make-vect .45 .9))
                      (make-segment (make-vect .55 .9) (make-vect .6 .9))
                      ))) 

(paint wave)

;; b
;; c
;; 省略

; 2.53
#lang racket

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

; 2.54
;; 式１と式２の両方から#t #tが帰ってきた場合は#t
;; それ以外が帰ってきた時は#fを返したい時はandを使う。
#lang racket

(define (equal? list1 list2)
  (let ((borth-list? (lambda (l1 l2)
                      (and (pair? l1) (pair? l2)))))
  (if (not (borth-list? list1 list2))
      (if (eq? list1 list2) #t #f)
      (and (equal? (car list1) (car list2))
           (equal? (cdr list1) (cdr list2))))))

;; TEST
(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f
              
; 2.55
#lang racket
;; 34にquoteの記載あり。
''abc

(quote 'abc)

(quote (quote abc))

(list (quote abc))

(quote (abc)) ;; <=> '(abc) <=> (list 'a 'b)

(list 'a 'b) ;'a'と'b'のリストである。

;; リストを表示する時に使う表現を使ってクオートの複合オブジェクトを表現可能。
;; 言い換えれば、quoteされたlistのシンタックスシュガーがある。
;; 上の(list 'a 'b)と同等の書き方。
'(a b)


(list 'quote '(a b c)) ;; ''abc
(list 'quote (list 'quote 'a)) ;'''abc

;; よって、(car ''abc)を評価した場合、'quoteが返却される。
;; 一つの可能性として、インタープリタ上ではlist構造になっており、quoteされたオブジェクトを評価
;; していることが予想される。
;; (list 'quote <expression>) <=> '<expression>なのかもしれない。

;; 注34の例に関しての実験
(define hoge (list 'car (list 'quote '(a b c))))
(car hoge)
(cadr hoge)

; 2.56
#lang racket
;; EERIV ALGORITHM
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ;; x + 3:: sum
        ((sum? exp)
         (make-sum
          (deriv (addend exp) var)
          (deriv (augend exp) var)))

         ;; x * 3 :: product
         ((product? exp)
          (make-sum
           (make-product (multiplier exp) (deriv (multiplicand exp) var))
           (make-product (multiplicand exp) (deriv (multiplier exp) var))))

         ;; x ** 3 :: exponentiation
         ((exponentation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))

         ;; other pattern :: error
         (else
          (error "unknown expression type: DERIV" exp ))))

;;;;;; CONSTRUCTORS, SELECTRS, so on;;;;;;;;;;

;;CHECKER
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (exponentation? x) (and (pair? x) (eq? (car x) '**)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

;; SELECTOR
(define (addend x) (cadr x))
(define (augend x) (caddr x))
(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (=number? exp num) (and (number? exp) (= exp num)))



(define (** base exp) (* base (** base (- exp 1))))


;; CONSTRUCTORS
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (** base exponent))
        (else (list '** base exponent))))

;;;;;; TEST
(deriv '(+ x 3) 'x)
;;1

(deriv '(* x y) 'x)
;;'y

(deriv '(* (* x y) (+ x 3)) 'x)
;;'(+ (* x y) (* (+ x 3) y))

(deriv '(* (* 3 y) (** x 3)) 'x)
;; '(* (* 3 y) (* 3 (** x 2)))
        
; 2.57
#lang racket
;; EERIV ALGORITHM
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ;; x + 3:: sum
        ((sum? exp)
         (make-sum
          (deriv (addend exp) var)
          (deriv (augend exp) var)))

         ;; x * 3 :: product
         ((product? exp)
          (make-sum
           (make-product (multiplier exp) (deriv (multiplicand exp) var))
           (make-product (multiplicand exp) (deriv (multiplier exp) var))))

         ;; x ** 3 :: exponentiation
         ((exponentation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))

         ;; other pattern :: error
         (else
          (error "unknown expression type: DERIV" exp ))))

;;;;;; CONSTRUCTORS, SELECTRS, so on;;;;;;;;;;

;;CHECKER
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (exponentation? x) (and (pair? x) (eq? (car x) '**)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

;; SELECTOR
(define (addend x) (cadr x))

(define (multiplier x) (cadr x))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

; 2.58

(define (=number? exp num) (and (number? exp) (= exp num)))



(define (** base exp) (* base (** base (- exp 1))))


;; CONSTRUCTORS
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (** base exponent))
        (else (list '** base exponent))))

(define (augend x)
  (if (null? (cddr x))
      0
      (cons '+ (cddr x))))

(define (multiplicand x)
  (if (null? (cddr x))
      1
      (cons '* (cddr x))))
 

;;;;;; TEST
(deriv '(* x y (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))
        
; 2.58.  
; a
#lang racket
;; EERIV ALGORITHM
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ;; x + 3:: sum
        ((sum? exp)
         (make-sum
          (deriv (addend exp) var)
          (deriv (augend exp) var)))

         ;; x * 3 :: product
         ((product? exp)
          (make-sum
           (make-product (multiplier exp) (deriv (multiplicand exp) var))
           (make-product (multiplicand exp) (deriv (multiplier exp) var))))

         ;; x ** 3 :: exponentiation
         ((exponentation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))

         ;; other pattern :: error
         (else
          (error "unknown expression type: DERIV" exp ))))

;;;;;; CONSTRUCTORS, SELECTRS, so on;;;;;;;;;;

;;CHECKER
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (exponentation? x) (and (pair? x) (eq? (cadr x) '**)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

;; SELECTOR
(define (addend x) (car x))
(define (augend x) (caddr x))
(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))
(define (base x) (car x))
(define (exponent x) (caddr x))

(define (=number? exp num) (and (number? exp) (= exp num)))



(define (** base exp) (* base (** base (- exp 1))))


;; CONSTRUCTORS
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (** base exponent))
        (else (list base '** exponent))))

 

;;;;;; TEST
(deriv '((3 * x) * (2 * y)) 'x)
;; '(+ (* x y) (* y (+ x 3)))

; b
;; 面倒なのでパス

; 2.59
#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2) '()))
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; union-set
;; 1. set1 がnullならばset2を返す。
;; 2. car set1がset2に含まれていないのならば、set２を返す。
;; 3. 含まれていないならば、(union-set (cdr set1) (adjoin-set (car set1) set2))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) set2)
        ( else (union-set
                (cdr set1)
                (adjoin-set (car set1) set2)))))

;; TEST
(union-set '() '())

(union-set '(1 2 3) '(1 2 3))

(union-set '(1 2 3) '(3 4 5))

; 2.60
#lang racket
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))

(define (remove-set x set)
  (cond ((null? set) '())
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (remove-set x (cdr set))))))
          
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) (remove-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(2 3 1 1 3 4) '(1 3 4))

;; 2.61
#lang racket
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((smallest (car set)))
        (cond ((= x smallest) set)
              ((< x smallest) (cons x set))
              (else (cons (car set) (adjoin-set x (cdr set))))))))
;;test
(adjoin-set 1 '(2 3 4 5))
(adjoin-set 5 '(1 2 3 4))
(adjoin-set 5 '(1 2 3 4 5))

; 2.62
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((smallest-set1 (car set1))
                    (smallest-set2 (car set2)))
                (cond ((= smallest-set1 smallest-set2) 
                       (cons smallest-set1 (union-set (cdr set1) (cdr set2))))
                      ((> smallest-set1 smallest-set2) 
                       (cons smallest-set2 (union-set set1 (cdr set2))))
                      ((< smallest-set1 smallest-set2)
                       (cons smallest-set1 (union-set (cdr set1) set2))))))))
; TEST
(union-set '(1 2 3) '(2 3 4 5))

; 2.63
#lang racket
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree ))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list )))))
  (copy-to-list tree '()))

; a
;; 同じリストを生成する。

; b
;; １の方が関数を２回づつ呼ぶ必要があるので遅い。

;; 2.64
#lang racket
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let (( left-size (quotient (- n 1) 2)))
        (let (( left-result
                (partial-tree elts left-size )))
          (let (( left-tree (car left-result ))
                (non-left-elts (cdr left-result ))
                (right-size (- n (+ left-size 1))))
            (let (( this-entry (car non-left-elts ))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size )))
              (let (( right-tree (car right-result ))
                    (remaining-elts
                     (cdr right-result )))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts ))))))))

(define (make-tree entry left right)
  (list entry left right))

(partial-tree '(1 3 5 7 9 11) 6)
; '((5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))

;;;; a

;;  partial-treeのeltsはソートされたリストが引数にとられることを想定します。
;; 引数に取られたリストは、まず一番左の要素を何にするかを決定するところからはじまります。
;; left-resultを再帰的に実行します。この時、再帰的に呼び出されるごとに木構造の階層が１つづつ下がっていきます。
;; 左の要素が決まると、右の要素の決定を行います。最左端は要素の一番小さい数が配置され、それに紐づく右の要素には２番目に小さい数が配置されます。
;; 角要素に対して幾つの要素を紐づけられるかは、left-size, right-sizeで決定します。
;; left-sizeは1/2ずつ減少するので、左の要素の最左端は１つ以下の要素しか保持できないようになっています。（最左端の場合、小さい数はないので、妥当です。）
;; これに対して最右端は１つ以下の要素しか持てません。
;; 左、中央、右という順番で要素が決まっていく。これを繰り返し、木構造を決定していく。

;;;; b
;; 1/2ずつ要素が現象するので、
;; T(n) = 2 * T(n/2) + O(1)
;; -> O(n)
 

;; 2.65
 (define (union-set tree1 tree2) 
   (list->tree (union-set-orderd-list (tree->list tree1) 
                          (tree->list tree2)))) 
  
 (define (intersection-set tree1 tree2) 
   (list->tree (intersection-set-orderd-list (tree->list tree1) 
                                 (tree->list tree2)))) 

 ;;2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        (( equal? given-key (key (car set-of-records )))
         (car set-of-records ))
        (( > given-key (key (car-set-of-records)))
         (lookup given-key (cdr set-of-records)))
        (( < given-key (key (car-set-of-records))) false)))

;; 2.67
#lang racket
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
        
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; 2.68
#lang racket
;;;;;;;;;; COMMON FUNCTIONS ;;;;;;;;;;;;;;
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol-of-tree tree x)
  (define (iter symbol-list x)
    (cond ((null? symbol-list) #f)
          ((equal? (car symbol-list) x) #t)
          (else (iter (cdr symbol-list) x))))
  
  (if (null? tree)
      #f
      (iter (symbols tree) x)))

(define (encode-symbol x set)
  (cond ((leaf? set) '())
        ((symbol-of-tree (left-branch set) x)
         (cons 0 (encode-symbol x (left-branch set))))
        ((symbol-of-tree (right-branch set) x)
         (cons 1 (encode-symbol x (right-branch set))))
        (else (error "character:" x "is not exist in tree"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; TEST SETES
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; TEST
(symbol-of-tree '() 'A)
(symbol-of-tree sample-tree 'A)
(symbol-of-tree sample-tree 'B)
(symbol-of-tree sample-tree 'C)
(symbol-of-tree sample-tree 'D)
(symbol-of-tree sample-tree 'E)

(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)

(encode '(A D A B B C A) sample-tree)
;> '(0 1 1 0 0 1 0 1 0 1 1 1 0)

; 2.69
#lang racket
(require racket/trace)
;;;; COMMON FUNCTION ;;;;;;;;
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond (( null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)(adjoin-set x (cdr set ))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs )))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs ))))))


(define (encode-symbol x set)
  (cond ((leaf? set) '())
        ((symbol-of-tree (left-branch set) x)
         (cons 0 (encode-symbol x (left-branch set))))
        ((symbol-of-tree (right-branch set) x)
         (cons 1 (encode-symbol x (right-branch set))))
        (else (error "character:" x "is not exist in tree"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-of-tree tree x)
  (define (iter symbol-list x)
    (cond ((null? symbol-list) #f)
          ((equal? (car symbol-list) x) #t)
          (else (iter (cdr symbol-list) x))))
  
  (if (null? tree)
      #f
      (iter (symbols tree) x)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

;;;;;;;; ANSWERS ;;;;;;;;;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
      ;; 全体でTreeを作成する際にListが２重になる。
      (car set)
      (let ((left (car set))
            (right (cadr set)))
        (let ((tree (make-code-tree left right)))
          (let ((next (adjoin-set tree (cddr set))))
            (successive-merge next))))))

(define test-tree (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
test-tree
(define message '(A B C D))
(define bits (encode message test-tree))
(define decode-message (decode bits test-tree))

message
bits
decode-message

; 2.70
#lang racket
(require racket/trace)
;;;; COMMON FUNCTION ;;;;;;;;
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond (( null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)(adjoin-set x (cdr set ))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs )))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs ))))))


(define (encode-symbol x set)
  (cond ((leaf? set) '())
        ((symbol-of-tree (left-branch set) x)
         (cons 0 (encode-symbol x (left-branch set))))
        ((symbol-of-tree (right-branch set) x)
         (cons 1 (encode-symbol x (right-branch set))))
        (else (error "character:" x "is not exist in tree"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-of-tree tree x)
  (define (iter symbol-list x)
    (cond ((null? symbol-list) #f)
          ((equal? (car symbol-list) x) #t)
          (else (iter (cdr symbol-list) x))))
  
  (if (null? tree)
      #f
      (iter (symbols tree) x)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

;;;;;;;; ANSWERS ;;;;;;;;;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
      ;; 全体でTreeを作成する際にListが２重になる。
      (car set)
      (let ((left (car set))
            (right (cadr set)))
        (let ((tree (make-code-tree left right)))
          (let ((next (adjoin-set tree (cddr set))))
            (successive-merge next))))))

;; 2.70
(define sample-list '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define sample-tree (generate-huffman-tree sample-list))

(define message1 '(GET A JOB))
(define message2 '(SHA NA NA NA NA NA NA NA NA))
(define message3 '(GET A JOB))
(define message4 '(SHA NA NA NA NA NA NA NA NA))
(define message5 '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP))
(define message6 '(SHA BOOM))

(define bit1 (encode message1 sample-tree))
(define bit2 (encode message2 sample-tree))
(define bit3 (encode message3 sample-tree))
(define bit4 (encode message4 sample-tree))
(define bit5 (encode message5 sample-tree))
(define bit6 (encode message6 sample-tree))

(decode bit1 sample-tree)
(decode bit2 sample-tree)
(decode bit3 sample-tree)
(decode bit4 sample-tree)
(decode bit5 sample-tree)
(decode bit6 sample-tree)


;; 2.71
;; n = 5
;;31
;;|\ 
;;15 16
;;|\
;;7 8
;;|\
;;3 4
;;|\
;;2 1

; 2.72
; パス

; 2.73
(define (deriv exp var)
  (cond (( number? exp) 0)
        (( variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)); op='deriv, type={sum, produce}
               (operands exp) var ))));; 演算式をgetしてoperandsを適用

; a.
; number?とvariable?はderivとインターフェイスをジェネリックに統一できないため.

; b.
(define (install-sum-deriv)
  (define (deriv exp var)
    (make-sum (make-product
                (deriv (addend exp) var)
                (deriv (augend exp) var))))
  (put 'deriv '+ deriv))


(define (install-product-deriv)
  (define (deriv exp var)
    (make-sum (make-product
                (deriv (addend exp) var)
                (deriv (augend exp) var))))
  (put 'deriv '* deriv))

; c.
#lang racket
;; define put and get
(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get 

(define (number exp num)
  (and (number? exp) (equal? exp num)))
(define (variable? exp) (symbol? exp))
(define (same-variable? exp var)
  (and (variable? exp) (variable? var) (eq? exp var)))
(define (** base exponent)
  (if (= exponent 0)
      1
      (* base (** base (- exponent 1)))))

;; Define Packages
(define (install-sum-deriv-package)
  (display "start install sum-deriv-package -> ")
  
  (define (sum-deriv exp var)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var)))
  (define (make-sum x1 x2)
    (cond ((number x1 0) x2)
          ((number x2 0) x1)
          ((and (number? x1) (number? x2)) (+ x1 x2))
          (else (list '+ x1 x2))))

  (define (augend exp) (car exp))
  (define (addend exp) (cadr exp))
  (put 'deriv '+ sum-deriv)
  (put 'make-sum '+ make-sum)
  
  'done
  )

(define (install-product-deriv-package)
  (display "start install product-deriv-package -> ")
  (define (product-deriv exp var)
    (make-sum (make-product
               (multiplicand exp)
               (deriv (multiplier exp) var))
              (make-product
               (deriv (multiplicand exp) var)
               (multiplier exp))))

  (define (make-product x1 x2)
    (cond ((or (number x1 0) (number x2 0)) 0)
          ((number x1 1) x2)
          ((number x2 1) x1)
          ((and (number? x1) (number? x2)) (* x1 x2))
          (else (list '* x1 x2))))

  (define (make-sum x1 x2) ((get 'make-sum '+) x1 x2))
  
  (define (multiplicand exp) (car exp))
  (define (multiplier exp) (cadr exp))

  (put 'deriv '* product-deriv)
  (put 'make-product '* make-product)
  'done
  )

(define (install-exponent-deriv-package)
  (display "start install exponent-deriv-package -> ")
  (define (exponent-deriv exp var)
    (make-product (exponent exp)
                  (make-exponent (base exp) (- (exponent exp) 1))))

  (define (make-exponent base exponent)
    (cond ((number exponent 1) exponent)
          ((number exponent 0) 0)
          ((number base 0) 0)
          ((and (number? base) (number? exponent)) (** base exponent))
          (else (list '** base exponent))))
  
  (define (make-product x y) ((get 'make-product '*) x y))
  (define (make-sum x1 x2) ((get 'make-sum '+) x1 x2))
  
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))

  (put 'deriv '** exponent-deriv)

  'done
  )

(install-sum-deriv-package)
(install-product-deriv-package)
(install-exponent-deriv-package)

(define (deriv exp var)
  (cond (( number? exp) 0)
        (( variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var ))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; TEST
(deriv '(* (* x y) (+ x 3)) 'x)
; '(+ (* x y) (* y (+ x 3)))
(deriv '(* (* 3 y) (** x 3)) 'x)
;; '(* (* 3 y) (* 3 (** x 2)))

;d.
; put内のmetnodの識別子とderivの順番を変えるだけ。

; 2.74
#lang racket
(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get 

(define (hoge-company)
  (define personnel-data
    ; (name (address sarary))
    '((taro (tokyo 1000)) (jiro (osaka 20000))))

  (define (get-record data key)
      (if (null? data) 
          '()
          (let ((person (car data)))
            (if (eq? (name person) key) 
                person
                (get-record (cdr data) key)))))

  (define (name person) (car person))
  (define (sarary person) (cadadr person))

  (put 'get-data 'hoge personnel-data)
  (put 'get-record 'hoge get-record)
  (put 'get-sarary 'hoge sarary)
)

(define (foo-company)
  (define personnel-data
    ; ((name) (address) (sarary))
    '(((nobi) (tokyo) (1000)) ((ken) (osaka) (20000))))

  (define (get-record data key)
      (if (null? data) 
          '()
          (let ((person (car data)))
            (if (eq? (name person) key) 
                person
                (get-record (cdr data) key)))))

  (define (name person) (car (car person)))
  (define (sarary person) (caaddr person))

  (put 'get-data 'foo personnel-data)
  (put 'get-record 'foo get-record)
  (put 'get-sarary 'foo sarary)
)

(hoge-company)
(foo-company)
;; Define Common
(define (get-data name)
  (let ((file (get 'get-data name)))
    (if (pair? file)
        (make-taged-data name file)
        (error name " company file does not exist"))))

(define (make-taged-data tag contents) (list tag contents))
(define (tag file) (car file))
(define (contents file) (cadr file))

;; a.
;; データに会社のタグがついている必要がある。
(define (get-record file name)
  (make-taged-data (tag file)
                   ((get 'get-record (tag file))
                    (contents file)
                    name)))

;; TEST
(get-record (get-data 'hoge) 'taro)
(get-record (get-data 'foo) 'nobi)

; b.
;; nameに紐づくデータに会社のタグがつく必要がある。
(define (get-sarary personnel-data)
  ((get 'get-sarary (tag personnel-data)) (contents personnel-data)))

(define hoge-taro (get-record (get-data 'hoge) 'taro))
(define foo-taro (get-record (get-data 'foo) 'taro))

(get-sarary hoge-taro)
;(get-sarary foo-taro)

; c.
(define (find-employee-record name files)
  (if (null? files)
       '()
       (let ((file (car files)))
         (let ((person-data (get-record file name)))
           (if (not (null? (contents person-data)))
               person-data
               (find-employee-record name (cdr files)))))))

;TEST
(define all-company-files (list (get-data 'hoge) (get-data 'foo)))
(find-employee-record 'taro all-company-files)
(find-employee-record 'nobi all-company-files)

;; d.
;; 会社のパッケージを作成し、putでメソッドを登録すればよい。


; 2.75
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op) 'real-part (* mag (cos ang)))
          ((eq? op) 'imag-part (* mag (sin ang)))
          ((eq? op) 'magnitude mag)
          ((eq? op) 'angle ang)
          (else (error "Unknown op: MAKE_FROM_REAL_IMAGE" op))))
  dispatch)

; 2.76
; 明示的ディスパッチ: 
; 新たに追加されるクラスがある場合:メソッドにそのクラスのデータに対する処理を条件分で付け足し、追記する。
; 新しいメソッドが追加された場合: メソッドを新規作成し、それぞれのクラスに対する処理を記述する。

; データ主導スタイル: 
; 新たに追加されるクラスがある場合: 新たにクラスの定義を追加する。追加しディスパッチが必要なメソッドはputで登録する。また、それぞれのデータに対してTagを定義する必要がある。
; 新しいメソッドが追加された場合: それぞれのクタスの定義に対して新たなメソッドを追記する。

; メッセージパッシングスタイル
; 新たに追加されるクラスがある場合: 新たなクラスに対するコンストラクタを作成。そこに、どのメッセージが来た場合にどのような振る舞いをするかを定義。

; 新しいメソッドが追加された場合: コンストラクタに新たなメソッドと、メソッドを識別するsymbolを記載する。

; 新たにクラスを追加する事が多い場合は「メッセージパッシングスタイル」を採用した方が良い。また、メソッドを追加する事が多い場合は、「データ主導スタイル」を採用するとよい。
; これは、データ主導スタイルの場合、クラスが増える度にtagの定義が必要になる。それに対して、メッセージパッシングの場合はコンストラクタを定義するだけでよい。しかし、メソッドがおおく追加される場合、メソッドパッシングの場合コンストラクタ情報が複雑になるので、データ主導スタイルを採用した方が良い。

; http://community.schemewiki.org/?sicp-ex-2.76


