;practice1-1
;やってみればわかるので省略

;practice1-2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;practice1-3
(define (square x) (* x x))
(define (square-sum x y)
  (+ (square x) (square y)))
(define (max-square-sum a b c)
  (square-sum
    (cond ((> a b) a)
          (else b)
          )
    (cond ((> c b) c)
          (else
            (cond ((> a c) a)
                  (else c))))))

;practice1-4
;演算子を手続きとして扱えるので
;bがマイナスの値の場合には、減算にすることで
;絶対値を加算した結果を返している。

;practice1-5
;適用順序評価の場合には式の結果が必要になるまでは評価されないので
;x=0の条件にマッチしてO(1)で終了する。
;正規順序評価では(p)の定義が再帰しているため、無限ループになって一生終了しない
