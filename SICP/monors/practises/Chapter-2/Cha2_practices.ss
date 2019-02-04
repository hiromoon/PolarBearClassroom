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
                
