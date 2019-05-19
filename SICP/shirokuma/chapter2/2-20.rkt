#lang racket

(define (same-parity x . list)
  (if (even? x) 
       (cons x (filter even? list))
       (cons x (filter odd? list))))

(define (filter func items)
  (let (( nil '() ))
    (if (null? items)
        nil
        (if (func (car items))
            (cons (car items) (filter func (cdr items)))
            (filter func (cdr items))))))

; test
(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; (2 4 6)
(same-parity 1)   ; (1)
(same-parity 2)   ; (2)