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
(define (make-rect x y w h))
(define (get-round rect))
(define (get-area rect))
