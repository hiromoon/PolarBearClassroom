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

;ニュートン法
;数値xの平方根の推定値としてyがあるとき、yとx/yの平均を取る
;取った値を次の推定値として計算を繰り返すことで近似値を得られる
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (square x) (* x x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;practice1-6
;正規樹所評価の場合に条件式の評価が遅延評価されないので
;無限ループになる

;practice1-7
;よくわかんなかったのですきっぷ

;practice1-8
(define (cbrt x)
  (cbrt-iter 1.0 x))
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))
(define (cube-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-iter (improve guess x) x)))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
