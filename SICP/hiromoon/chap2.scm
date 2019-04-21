
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

;practice2-42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
(define empty-board (list))
(define (safe? k positions)
  (define (row? row positions)
    (cond ((null? positions) #t)
          ((= row (car positions)) #f)
          (else (row? row (cdr positions)))))
  (define (upper? row positions)
    (cond ((null? positions) #t)
          ((= (- row 1) (car positions)) #f)
          (else (upper? (- row 1) (cdr positions)))))
  (define (lower? row positions)
    (cond ((null? positions) #t)
          ((= (+ row 1) (car positions)) #f)
          (else (lower? (+ row 1) (cdr positions)))))
  (let ((queen (car positions))
        (rest (cdr positions)))
    (every? 
      (list
        (row? queen rest)
        (upper? queen rest)
        (lower? queen rest)))))


(define (every? l)
  (cond ((null? l) #t)
        ((not (car l)) #f)
        (else (every? (cdr l)))))

;practice2-43
;枝刈りをせずにすべてのパターンを走査することになるから(?)
;n^n*T

;practice2-44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;practice2-45
(define (split f g)
  (define (do-split painter n)
    (if (= n 0) 
      painter
      (let ((smaller (do-split painter (- n 1))))
        (f painter (g smaller smaller)))))
  do-split)

;practice2-46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s) 
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;practice2-47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;practice2-48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;practice2-49
(define (segments->painter segment-list) 
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))
;a. 指定された枠の輪郭を描くペインタ
(define (round-painter frame)
  (segments->painter
    (list
      (make-segment
        (origin-frame frame)
        (edge1-frame frame))
      (make-segment
        (origin-frame frame)
        (edge2-frame frame))
      (make-segment
        (edge1-frame frame)
        (add-vect
          (edge1-frame frame)
          (edge2-frame frame)))
      (make-segment
        (edge2-frame frame)
        (add-vect
          (edge1-frame frame)
          (edge2-frame frame))))))
;b. 枠の対角線同士をつないで"X"を描くペインタ
(define (diagonal-painter frame)
  (segments->painter
    (list
      (make-segment
        (origin-frame frame)
        (add-vect
          (edge1-frame frame)
          (edge2-frame frame)))
      (make-segment
        (edge1-frame frame)
        (edge2-frame frame)))))
;c. 枠の辺の中点をつないでひし形を描くペインタ
(define (diamond-painter frame)
  (let ((point1
          (scale-vect 
            (sub-vect
              (origin-frame frame)
              (edge1-frame frame)) 
            0.5))
        (point2
          (scale-vect 
            (sub-vect
              (origin-frame frame)
              (edge2-frame frame)) 
            0.5))
        (point3
          (add-vect
            (edge2-frame frame)
            (scale-vect (edge1-frame frame) 0.5)))
        (point4
            (edge1-frame frame)
            (scale-vect (edge2-frame frame) 0.5)))
    (segments-painter
      (list
        (make-segment point1 point2)
        (make-segment point2 point3)
        (make-segment point3 point4)
        (make-segment point4 point1)))))
;d. waveペインタ
;めんどいからすきっぷ

;practice2-50
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

;practice2-51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top 
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 1.0 1.0))) 
          (paint-bottom
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below painter1 painter2) 
  (rotate90
    (beside
      (rotate270 painter1)
      (rotate270 painter2))))

;practice2-52
;すきっぷ

;practice2-53
; '(a b c)
; '((george))
; '((y1 y2))
; '(y1 y2)
; #f
; #f
; '(red shoes blue socks)

;practice2-54
(define (equal? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? (car l1)) (pair? (car l2)))
           (if (equal? (car l1) (car l2))
             (equal? (cdr l1) (cdr l2))
             #f))
        ((or (pair? (car l1)) (pair? (car l2))) #f)
        (else (equal? (cdr l1) (cdr l2)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;practice2-55
;http://community.schemewiki.org/?sicp-ex-2.55

;practice2-56
(define variable? symbol?) ;e は変数か?
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))) ;v1 と v2 は同じ変数か?

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)));e は和か?
(define addend cadr) ;和 e の加数
(define augend caddr) ;和 e の被加数
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2)))) ;a1 と a2 の和を構築する

(define (product? e) ;e は積か?
  (and (pair? e) (eq? (car e) '*)));e は和か?
(define multiplier cadr) ;積 e の乗数
(define multiplicand caddr) ;積 e の被乗数
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2)))) ;m1 と m2 の積を構築する

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        ((and (number? base) (number? exponent)) (exponentiation base exponent))
        (else (list '** base exponent))))

(define (exponentiation base exponent)
  (define (iter b e acc)
    (if (= e 0)
      acc
      (iter b (- e 1) (* acc b))))
  (iter b e 1))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)  
                          (make-product  
                            (make-product  
                              (exponent exp) 
                              (make-exponentiation (base exp) 
                              (make-sum (exponent exp) -1)))                                                                                                 
                            (deriv (base exp) var)))
        (else
          (error "unknown expression type: DERIV" exp))))

;practice2-57
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)));e は和か?
(define addend cadr) ;和 e の加数

(define (augend s) ;和 e の被加数
  (accumulate make-sum '0 (cddr s)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2)))) ;a1 と a2 の和を構築する

(define (product? e) ;e は積か?
  (and (pair? e) (eq? (car e) '*)));e は和か?
(define multiplier cadr) ;積 e の乗数
(define (multiplicand p)
  (accumulate make-product '1 (cddr p))) ;積 e の被乗数
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2)))) ;m1 と m2 の積を構築する

;practice2-58
;a
(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))

(define addend car)
(define augend caddr)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else '(a1 + a2))))

(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))

(define multiplier car)
(define multiplicand caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else '(m1 * m2)))) ;m1 と m2 の積を構築する

;b
;パス

;practice2-59
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (addjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (union-set set1 set2)
  (accumulate addjoin-set set2 set1))

;practice2-60
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set))))) ;変わらない

(define (adjoin-set x set)
  (cons x set)) ;なにも考えずに追加するだけなのでO(1)

(define (union-set set1 set2)
  (append set2 set1)) ;なにも考えずに追加するだけなのでO(1)

(define (uniq set)
  (accumulate
    (lambda (x acc)
      (if (element-of-set? x acc)
        acc
        (cons x acc))
    '()
    set)))

(define (intersection-set set1 set2)
  (let ((uniq-set1 (uniq set1))
        (uniq-set2 (uniq set2)))
    (accumulate (lambda (x acc)
                  (if (element-of-set? x uniq-set2)
                    (cons x acc)
                    acc))
                '()
                uniq-set1)))

;adjoinだけは早いので、要素の書き込みが多い場合に向いている(?)


;practice2-61
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (addjoin-set x set)
  (cond ((null? set) (cons x))
        ((= x (car set)) set)
        ((< x (car set) (cons x set)))
        (else (cons (car set) (addjoin-set x (cdr set))))))

;practice2-62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set2) (car set1))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

;practice2-63
;図2-16
(define a '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define b '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define c '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1
                    (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

;a
(tree->list-1 a)
(tree->list-2 a)
(tree->list-1 b)
(tree->list-2 b)
(tree->list-1 c)
(tree->list-2 c)
;全部'(1 3 5 7 9 11) で同じ結果を返す

;b
;線形再帰なのでtree->list-1のほうが遅い気がする。
;Oは↓の通りらしい
;tree->list-1 O(n * log n)
;tree->list-2 O(n)

;practice2-64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

;要素のリストと要素の数を受け取る
;要素の数が0の場合は、葉に当たるので空リストを追加して返す
;要素の数が1以上の場合
; 親ノードを引いた半分が左の木の要素数になるのでleft-sizeとして保存する
; partial-tree に要素とleft-sizeを渡して、左の部分木を作る(left-tree)
; (left)

;practice2-65
;union 和集合
(define (union-set t1 t2)
  (list->tree (union-set-list
                (tree->list t1)
                (tree->list t2))))

;intersection 積集合
(define (intersection-set t1 t2)
  (list->tree (intersection-set-list
                (tree->list t1)
                (tree->list t2))))

;practice2-66
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (lookup tree key)
  (cond ((null? tree) #f)
        ((= (entry tree) key) (entry tree))
        ((> (entry tree) key) 
         (lookup (left-branch tree) key))
        (else (lookup (right-branch tree) key))))

;practice2-67
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)
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
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;'(A D A B B C A)

;practice2-68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
    '()
  (if (not (include? symbol (symbols tree)))
    (error "bad symbol: ENCODE-SYMBOL: " symbol)
    (if (include? symbol (symbols (left-branch tree)))
      (append '(0) (encode-symbol symbol (left-branch tree)))
      (append '(1) (encode-symbol symbol (right-branch tree)))))))

(define (include? e li)
  (cond ((null? li) #f)
        ((eq? (car li) e) #t)
        (else (include? e (cdr li)))))

(encode '(A D A B B C A) sample-tree)

;practice2-69
(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car piar)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (successive-merge leaf-set)
  (if (null?  leaf-set))
    (car leaf-set)
    (make-code-tree
      (succesive-merge (cdr leaf-set))
      (car leaf-set)))

;practice2-70
(define huffman-tree
  (generate-huffman-tree
    (list
      '(a 2)
      '(Get 2)
      '(Sha 3)
      '(Wah 1)
      '(boom 1)
      '(job 2)
      '(na 16)
      '(yip 9))))
(encode '(Get a job) huffman-tree) ;12bit
(encode '(Sha na na na na na na na na) huffman-tree) ;62bit
(encode '(Get a job) huffman-tree) ; 12bit
(encode '(Sha na na na na na na na na) huffman-tree) ;62bit
(encode '(Wah yip yip yip yip yip yip yip yip yip) huffman-tree) ;65bit
(encode '(Sha boom) huffman-tree) ; 7bit

;practice2-71
; 2^n - 1 bit必要
; お絵かきは省略

;practice2-72
;O(n * log n) ~ O(n^2) らしい(skip

;practice2-73

;a
;実際の計算部分をジェネリックな式に移譲している
;インタフェースを通してsymbolでアクセスするので実装の入れ替えができる
;number? variable? は、移譲先を決定するために利用しているのでデータ手動にできない(?)

;b
(define install-add-package 
  ; 定義類(ry
  (define (tag x) (attach-tag '+))
  (put 'derive '+ derive-sum)
  (put 'derive '(+)
       (lambda (x y) (tag (make-sum x y))))
  'done)

;c
;bと大体一緒なのでスキップ

;d
;putする際のタグを入れ替える

;practice2-74
;a
(define (attach-tag type-tag content) (cons type-tag content))
(define (get-record employee-name file)
  (attach-tag (division file) 
              ((get 'get-record (division file)) employee-name file)))

;b
(define (get-salary record)
  (let ((record-type (car record))
        (record-content (cdr record)))
    ((get 'get-salary record-type) record-content)))

;c
(define (find-employee-record employee-name file-list)
  (if (null? file-list)
    #f
    (let ((file (car file-list))
          (record (get-record employee-name file)))
      (if (record)
        record
        (find-employee-record employee-name (cdr file-list))))))

;d
;新しい会社用の人事ファイル型を追加する必要がある
;人事ファイル型には,get-recordとget-salaryメソッドを準備する

;practice2-75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else error "Unknown op: MAKE-FROM-MAG-ANG" op)))
  dispatch)

;practice2-76
;明示的ディスパッチによるジェネリック演算
;  型を追加するたびに実装とディスパッチの処理を各演算に呼び出す演算を記述する
;  演算を追加する時は、ディスパッチの処理を記述する

;データ主導
;  新しい型を実装する時には、実装とインストール処理を記述する必要がある
;  新しい演算を追加する時には、同じく実装とインストール処理の記述が必要

;メッセージパッシング
;  新しい型を実装するたびにすべての実装が必要
;  新しい演算を追加するためには、全ての型に演算を追加する必要がある

;型を追加するのはメッセージパッシング(OOP)
;演算を追加するのはデータ主導(FP)

;practice2-77
;magnitudeがインタフェースとして外部へ公開されていないから
;complex と rectanglerのmagnitudeを呼ぶので2回

;practice2-78
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

;practice2-79
(define (equ? x y)
  (apply-generic 'equ? x y))

(put 'equ? (scheme-number scheme-number) =)
(put 'equ? (rational rational) 
     (lambda (x y) 
       (=
         (* (denom x) (numer y))
         (* (denom y) (numer x)))))

(put 'equ? (complex complex) 
     (lambda(x y)
       (and
         (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y)))))

;practice2-80
(define (=zero? x)
  (apply-generic '=zero? x))
(put '=zero? (scheme-number schem-number) 
     (lambda (x)
       (= 0 x)))
(put '=zero? (rational rational) 
     (lambda (x)
       (= 0 (numer x))))
(put '=zero? (complex complex)
     (lambda (x)
       (= 0 (real-part x) (imag-part x))))

;practice2-81
;a
;見つからない場合にも、複素数型にキャストしようとして無限ループになる


;b
;Louisのコードは動作しないし、apply-genericはそのままで正しい動作をするはず

;c
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (equal? type1 type2) 
              (error "No method for these types"
                     (list op type-tags))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2-> t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else (error "No method for these types"
                                   (list op type-tags)))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;practice2-82
(define (true-map proc sequence)
  (define (iter proc sequence acc)
    (if (null? sequence) 
      (reverse acc)
      (let ((item (proc (car sequence))))
        (if item
          (iter proc (cdr sequence) (cons item acc))
          #f))))
  (iter proc sequence '()))
(define (apply-generic op . args)
  (define (iter type-tags args)
    (if (null? type-tags)
      (error "No method for these types")
      (let ((type1 (car type-tags)))
        (let ((filtered-args
                (true-map (lambda (x)
                            (let ((type2 (type-tag x)))
                              (if (eq? type1 type2)
                                x
                                (let ((t2->t1 (get-coercion type2 type1)))
                                  (if (null? t2->t1)
                                    #f
                                    (t2->t1 x))))))
                          args)))
          (or filtered-args 
              (iter (cdr type-tags) args))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
        (apply proc (map contents args))
        (apply applygeneric (cons op (iter type-tags args)))))))
