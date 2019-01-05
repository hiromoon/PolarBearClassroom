;practice2-1
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
