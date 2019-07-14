#lang racket

(define nil '())

(define (foreach f list)
  (define (foreach-itr f list result)
    (if (null? list)
        #t
        (foreach-itr f (cdr list) (f (car list)))))
  (foreach-itr f list nil))


; test
; #tは表示されてほしくない。。
(foreach (lambda (x) (newline)
            (display x))
          (list 57 321 88)) ; 57 321 88