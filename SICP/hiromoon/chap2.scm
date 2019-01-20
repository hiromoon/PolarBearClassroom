
(define (make-rat n d)
  (if (and (> 0 n) (> 0 d))
    (cons (abs n) (abs d))
    (if (and (< 0 n) (> 0 d))
      (cons (* -1 n) (abs d))
      (cons n d))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;practice2-2
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (midpoint-segment segment)
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  (cons
    (average
      (car (start-segment segment))
      (car (end-segment segment)))
    (average 
      (cdr (start-segment segment))
      (cdr (end-segment segment)))
      ))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point
  (midpoint-segment
    (make-segment
      (make-point 1 1)
      (make-point 3 3))))

;practice2-3
;first implementation
(define (make-rect left-bottom right-top) 
  (cons 
    (make-segment
      left-bottom
      (make-point (x-point right-top) (y-point left-bottom)))
    (make-segment
      left-bottom
      (make-point (x-point left-bottom) (y-point right-top)))))

(define (pow x exp)
  (if (= exp 0)
    1
    (* x (pow x (- exp 1)))))

(define (length segment) 
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (sqrt
      (+
        (pow 
          (- (x-point p2) (x-point p1))
          2)
        (pow
          (- (y-point p2) (y-point p1))
          2)))))

(define (width rect)
  (length (car rect)))
(define (height rect)
  (length (cdr rect)))

;second implementation
(define (make-rect left-bottom width height)
  (cons left-bottom (cons width height)))

(define (width rect)
  (car (cdr rect)))

(define (height rect)
  (cdr (cdr rect)))


(define (round rect)
  (*
    2
    (+ (width rect) (height rect))))

(define (area rect)
  (* (width rect) (height rect)))


;practice2-4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
;(car (cons x y))
;(car (lambda (m) (m x y)))
;(lambda (lambda (p q) p) ((lambda (p q) p) x y))
;((lambda (p q) p) x y)
;((x y) x)
;x

;practice2-5
(define (cons a b)
  (*
    (pow 2 a)
    (pow 3 b)))

(define (car n)
  (if (not (= (remainder n 2) 0))
    0
    (+ 1 (car (quotient n 2)))
  ))
(define (cdr n)
  (if (not (= (remainder n 3) 0))
    0
    (+ 1 (car (quotient n 3)))
  ))

;practice2-6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (plus f g)
    (lambda (h) 
      (lambda (x)
        ((f h) ((g h) x)))))

(define (succ n) (+ n 1))

(((plus one two) succ) 0) ;3
(((plus two two) succ) 0) ;4

;practice2-7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (car interval))
(define (lower-bound interval)
  (cdr interval))

;practice2-8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;practice2-9
(define (interval-width interval) 
  (/
    (- (upper-bound interval) (lower-bound interval))
    2))
;TODO: 発展問題なので2-16までは一旦スキップ

;practice2-17
(define (last-pair l)
  (if (null? (cdr l))
    (list (car l))
    (last-pair (cdr l))))

(last-pair (list 23 72 149 34))

ubsets s) (if (null? s)
(list nil)
(let ((rest (subsets (cdr s))))
(append rest (map ⟨??⟩ rest)))))practice2-18
(define (reverse l)
  (if (null? l)
    null
    (cons
      (car l)
      (reverse (cdr l)))))

(reverse (list 1 4 9 16 25))

;practice2-19
;リストに対して全探索しているため
;リストの順番によって答えに影響を与えない
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values)) 
                 coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)

;practice2-20
(define (same-parity n . ns)
  (define (iter is_even ns acc)
    (if (null? ns)
      acc
      (if (= is_even (remainder (car ns))) 
        (iter is_even (cdr ns) (append acc (list (car ns))))
        (iter is_even (cdr ns) acc)))
  (iter (remainder n) ns (list n)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;practice2-21
(define (square-list items)
  (if (null? items)
    nil
    (cons
      (square (car items))
      (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))

;practice2-22
;1. 処理した結果をリストの先頭に追加して反復しているから
;2. 値と次のノードへの参照が逆転して、リストの構造にならないから

;practice2-23
(define (for-each fun l)
  (define (iter fun l)
    (fun (car l))
    (if (null? (cdr l))
      0
      (iter fun (cdr l))))
  (iter fun l))

;practice2-24
;末尾だけ参照になっていないリスト構造なのは明白なのでスキップ

;practice2-25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(cadr (cadr (cadr (cadr (cadr (cadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))

;practice2-26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;'(1 2 3 4 5 6)
(cons x y)   ;'((1 2 3) 4 5 6)
(list x y)   ;'((1 2 3) (4 5 6))

;practice2-27
(define (reverse l)
  (define (iter xs acc) 
    (if (null? xs)
      acc
      (iter
        (cdr xs)
        (cons (car xs) acc))))
  (iter l null))

(define (deep-reverse l) 
  (define (iter xs acc) 
    (if (null? xs)
      acc
      (iter
        (cdr xs)
        (cons
          (if (pair? (car xs)) 
            (deep-reverse (car xs))
            (car xs))
          acc))))
  (iter l null))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)

;practice2-28
(define (fringe xs)
  (define (exec xs)
    (define (iter xs acc)
      (if (null? xs) 
        acc
        (iter
          (cdr xs)
          (append
            (if (pair? (car xs))
              (exec (car xs))
              (list (car xs)))
            acc))))
    (iter xs (list)))
  (reverse (exec xs)))

(define x (list (list 1 2) (list 3 4))
(fringe x)
(fringe (list x x))

;practice2-29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a
(define left-branch car)
(define right-branch cdr)

(define branch-length car)
(define branch-structure cdr)

;b
(define (total-weight mobile)
  (+
    (if (pair?  (branch-structure (left-branch mobile)))
      (total-weight (branch-structure (left-branch mobile)))
      (branch-structure (left-branch mobile)))
    (if (pair?  (branch-structure (left-branch mobile)))
      (total-weight (branch-structure (left-branch mobile)))
      (branch-structure (left-branch mobile)))))

;c
(define (balanced? mobile) 
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (= 
      (* (branch-length left) (total-weight (branch-structure left)))
      (* (branch-length right) (total-weight (branch-structure right))))))

;d
;特に変更は必要ない

;practice2-30
(define (square-tree tree)
  (define (square x) (* x x))
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(define (square-tree tree)
  (define (square x) (* x x))
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;practice2-31
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(define (tree-map func tree) 
  (map (lambda (sub-tree)
         (if (pair? sub-tree) 
           (tree-map func sub-tree)
           (func sub-tree)))
       tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;practice2-32
(define (subsets s)
  (if (null? s)
    (list null)
    (let ((rest (subsets (cdr s))))
      (append rest
              (map
                (lambda (x) (cons (car s) x))
                rest)))))
;http://community.schemewiki.org/?sicp-ex-2.32
;リストの先頭から順にドリルダウンしていって、
;先頭とそのサブセットの組み合わせをmapで作成している

;practice2-33
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (* x x) (list 1 2 3)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (_ x) (+ x 1)) 0 sequence))

(length (list 1 2 3))

;practice2-34
(define (honer-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(honer-eval 2 (list 1 3 0 5 0 1))

;practice2-35
(define (count-leaves t)
  (accumulate
    +
    0
    (map (lambda (b)
           (cond ((null? b) 0)
                 ((not (pair? b)) 1)
                 (else (count-leaves b))))
         t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x) ;4

;practice2-36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    null
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s); (22 26 30)

;practice2-37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mj) (map * v mj)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi ni) (map * mi ni)) m n)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))

(dot-product (car m) (car m))
(matrix-*-vector m (car m)) 
(transpose m)
(matrix-*-matrix m n)

;practice2-38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence)) 

(fold-right / 1 (list 1 2 3)) ;1+1/2
(fold-left / 1 (list 1 2 3)); 1/6
(fold-right list nil (list 1 2 3)) ;(1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ;(((() 1) 2) 3)
; 交換則: 計算の順序が変わっても同じ答えになる

;practice2-39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append (list y) x)) null sequence))

;practice2-40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval start end)
  (define (iter n acc)
    (if (> n end)
      acc
      (iter (cons n acc))))
  (reverse (iter start null)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

;practice2-41
(define (flatmap proc seq)
  (accumulate cons null (map proc seq)))

(define (find-sum ans seqs)
  (filter (lambda (ns) (= (accumulate + 0 ns) ans)) seqs))

(define seq (list (list 1 2 3) (list 2 3 4)))
(find-sum 6 seq)
