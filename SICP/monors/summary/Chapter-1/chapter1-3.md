# 1.3 高階手続きによる抽象の定式化
下記の様に、手続きは特定の数値から独立した形で記述する抽象化であることをこれまでみてきました。
```lisp
(define (cube x) (* x x x))
```
しかし、下記の様に記述することもできます。
```lisp
(* 3 3 3)
(* x x x)
(* y y y)
```
この記述方法では、３乗という概念を抽象化したものを持たないため、毎回記述しなければならない。
この様に、プログラミングにおいてあるパターンを概念として表現するのは大きな意味を持ちます。
しかし、数値計算であっても、もし引数が数値であるような手続きしか作れないのだとしたら、抽象化の能力は限定されたものになるでしょう。
同じプログラミングパターンがいくつものことなる手続きに利用されることは、よくあります。
この様なパターンを概念として表現するためには、下記に対応する必要があります。
* 手続きを引数として受け取る。
* 手続きを戻り値として返す。

この様に、手続きを操作する手続きは高階手続きと呼ばれます。

## 1.3.1 引数としての手続き
例えば、下記のお通りa ~ bまでの合計値を合算するプログラムがあったとする。
```lisp
(define (sum-interger a b)
  (if (> a b)
      0
      (+ a (sum-interger (+ a 1) b))))
```
この様な計算がある時、下記の様に抽象化が可能である。
```lisp
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))
```
これらに方法としては、`term`に任意の関数を代入し、
`next`には、aをどの様にbに近づけるかを定義する。
例えば、下記の様な関数があったとします。

```lisp
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 ( * a ( + a 2)))
         (pi-sum (+ a 4) b))))
```
この関数は、`sum`を使用することで下記の通り書き換えられる。
```lisp
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))

  (define (pi-next x)
    (+ a 4))

  (sum pi-term a pi-next b))
```
※ `pi-term``pi-next`に関しては他の関数では役にたちそうにないため、
  関数内部に定義を押し込めてしまっている。
## 1.3.2 lambdaを使った手続きの構築
### lambdaを使用した記述方法
lambdaを使用することで、名前空間を使用することなく関数を作成することが可能。
例えば、pi-sumて鼓は次の様に補助手続きを定義しないで表現できます。
```lisp
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
        a
       (lambda (x) (+ x 4))
        b))
```
また、値として手続きを持つ他の式と同じ様に、lambda式は次の様に複合式の中で演算子として使用する事ができます。
```lisp
(lambda (x y z) (+ x y (square z)) 1 2 3)
; 12
```
### letを使用して局所変数を作成する。
lambfadの別の使い方として、局所変数を作成するというものがあります。
局所関数を使用したい場合のシチュエーションとは下記の場合です.
f(x, y) = x(1 _ xy)^2 - y(1 - y) + (1 + xy)(1 - y)

の様な関数を下記の様に表現したい場合です.

a = 1 + xy
b = 1 - y
f(x, y) = xa^2 + yb + ab

これをlambdaを使用して表すと次の様になります。

```lisp
(define (f a b)
  (lambda (a b) 
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (+ 1 (* x y))
  (- 1 y))
``` 
この概念は便利なので、letという特殊形式でさらに手軽に使えるようになっています。
letをつかうと、手続きfは次の様にかけます。
```lisp
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
```
この記述方法は、lambdaのシンタックスシュガーです。
let式で指定された変数のスコープがletの本体である。
例えば、下記の式の場合、局部関数の定義部分は外部の変数を採用するのに対して、let本体は局部関数の定義を使用します。
```lisp
(let ((x 3)
         (y (+ x 2)))
  (* x y))
```
## 1.3.3 汎用手法としての手続き
関数の零点を不動店を見つける汎用手法について検討し、これらが直接表現できることを示します。

### 区分二分法によって方程式の根を求める。
f(x) = 0である点を求めるための方法である。
必要なステップ数の増加オーダーは、元の区間の長さをL、許容誤差（"十分に小さい”とみなす区分の大きさ)をTとして、ø(log(L/t))になります。
```lisp
(define (serch f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
              (cond ((positive? test-point)
                     (search f neg-point midpoint))
                    ((negapoint? test-point)
                     (search f midpoint pos-point))
                    (else midpoint))))))
```
両端が十分に近いかどうかをテストするには、前回平方根をとめるるのに使ったのと同じ手続きが使います。
```lisp
(define (clase-enough? x y) (< (abs (- x y)) 0.001))
```

searchを直接使用すると面倒な事があります。
fに適用される関数の種類によって、neg-positとpos-positの順番が変化します。
これを解決してくれる様な関数が必要となります。
```lisp
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
       (cond ((and (positive? a-value) (negative? b-value))
              (serch f a b))
             ((and (negative? a-value) (negative? a-value))
              (serch f b a))
             (else 
              (error "Values are not of opposite sign" a b)))))
```

### 不動点を求める。
不動店とは、f(x) = xとなる様な点のことをいう。
この近似値は、下記のようにfを繰り返し適用していくやり方でもとめる事ができる。
f(x), f(f(x)), f(f(f(x))), ...

変更前後の平均値が十分小さいもの場合、適用をやめる。
この考え方をしようして、関数と初期値を入力して、その関数の不動点の近似値を生成する手続きを作る事ができます。

```lisp
(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
      tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))
```
このような方法は、ある基準値を満たす数値を繰り返し改善することで求めるという考えに基づいています。

先に求めた、平方根に関しても、y^2 = xを満たすyを検索するのと同義です。
y -> x / y <=> y -> 1/2 (y + x / y)
* 推定値に対してこれを繰り返せば、近似できるというものである。
* 実際にはイコール関係ではなく、変換関係である。

### 用語
* 不動点探索
  * 推定を繰り返して基準値の推定の精度を高めていく。
* 平均緩和法
  * 不動点探索法で連続した近似値の平均をとるというアプローチ

## 1.3.4 戻り値としての手続き

