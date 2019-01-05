# 2-1: 有理数の数値演算
有理数の数値演算の実装を行う。
有理数を用いた加算、除算、乗算、減算を行う。

有利数を扱う場合の書く関数を下記の様に定義する。
* **(make-rat <n> <d>)** ... 分数が<n>で分数が整数<d>である有理数を返す。
* **(numer <x>)** ... 有理数<x>の分子を返す。
* **(denom <x>)** ... 有理数<x>の分母を返す。


```lisp
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x))
            (* (denom x) (denom y)))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x))
            (* (denom x) (denom y)))))               
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)
     (* (numer y) (denom x)))))

```

### pair
二つのデータを尊くするためのデータ
* **cons** ... ペアーの構築を行う。
* **car** ... ペアの先頭を取り出す。
* **cdr** ... ペアの末尾を取り出す。

使用方法の例
```lisp
(define x (dons 1 2))
(car x)
1
(cdr x)
2
```

これを用いることでリスト構造を構築可能になる。
2.2でそれを学ぶ。

### 有理数を表現する
有理数は、**cons**を使用して下記の様に定義することが可能である。
```lisp
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
```
また、計算結果を表示するために、有理数を分数、スラッシュ、分母として表示することにします。
```lisp
(difine (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))
```


