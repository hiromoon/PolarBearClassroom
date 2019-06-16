#lang racket

(define x (list 1 (list 2 (list 3 4))))
(length x)

; インタプリタ表示結果
  ; (1 (2 (3 4)))|

; 箱点と木はかみで

; 写経
#|(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr1 x))))))

(count-leaves x)|#

