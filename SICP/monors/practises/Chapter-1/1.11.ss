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
(
    define (fr a b c count)
        (cond ((= count 1) a)
              ((= count 2) b)
              ((= count 3) c)
              (else (+
                        (* 3 (f c))
                        (* 2 (f b))
                        (f a)
              )
              )
        )
)
(display (f 3))
