#lang racket

(define nil '())

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
(define (square x) (* x x))

; 先に評価した要素を先頭にしている。
(square-list1 (list 1 2 3 4)) ; (16 9 4 1)

(define (square-list2 items)
  (define (iter things answer)a
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;　値と次のペアへの参照がになっている為、リスト構造にならない為。
(square-list2 (list 1 2 3 4))