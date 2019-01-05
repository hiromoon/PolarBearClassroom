#lang racket
;; # 練習問題1.11
;; 再帰
(
  define (f n)
  (cond ((< n 3) n)
        (else (+ ( f (- n 1)) 
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))
                )
            )
  )
)

(display (f 3))
(display "\n")
; 反復
; <- a b c - と数値が移動する。そのため、aの結果を返すとすれば、3つぶんだけ猶予が生まれる。
(define (f n)
  (define (f-iter a b c count)
    (if (= count 0)
        a
        (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))

(f 1)
(f 2)
(f 3)
(f 4)
