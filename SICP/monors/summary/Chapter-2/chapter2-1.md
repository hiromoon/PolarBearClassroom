#２章の目的は
単純なデータオブジェクト(数値、文字）を組み合わせて、複合オブジェクト（分数、有理数、区間など）の作成を行う。

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
分数を表すためのロジックは下記の通りです。
```lisp
#lang racket
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
; 1 / 2

(define one-third (make-rat 6 9))
; 6 / 9
; -> 既約されない。。
```
しかし、最後の例に記載した通りこの方法では既約が行われません。
既約を行うためには、`make-rat`に一手間加える必要があります。
`gcd`を使用して最大公約数を算出し、２つの数をそれで乗算するのです。

```lisp
#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

; ******* 既約できる様に変更 *******
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; *******************************

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 6 9))
; 2/ 3

```

### データとはなにか
実際データは手続きを使用して表現することができます。
先ほど使用した`cons`に関して下記の様に実装が可能です。
```lisp
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m)))
    (dispatch)))

(define (car z) (z 0))
(define (cdr z) (z 1))
``` 
このように定義されたデータと、primitiveのデータを区別することは不可能である。
このように手続きを用いたデータの表現は、私たちのプログラミングのレパートリーの中で中心的な役割を果たします。このプログラミングスタイルは**メッセージパッシング**呼ばれ、後の章で出てきます。

### 発展問題： 区分演算
精度に範囲がある場合、上限と下限の**範囲**という概念が付いてきます。
ex) 2.34 (± 2.3)
この範囲を数値演算のセットとして**区分演算**を実装することを行う。

* **和**
```lisp
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
```

* **乗算**
```lisp
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))
```

* **除算**
```lisp
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (lower-bound y))
                  (/ 1.0 (upper-bound y)))))
```
# 2.2.1 列の表現
ペアを使って構築できる便利な構造のひとつに列がある。
(オブジェクトの順序つき集合です。）
Lispでは単結合リストはlis()で宣言可能である。

# 2.2.2 リスト演算
ペアを使って要素の列をリストとして表現するやり方は、リストを'cdrダウン'していくという
確立されたプログラミングテクニックとセットになっています。
例えば、list-ref手続きは引数としてリストと引数nを取り、リストn番目のものを返します。

```lisp
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
```

List全体をcrdダウンすることもよくあります。
そのためにSchemeは基本z述語nil?を持っています。

```lisp
(define (length items)
  (if (null? items)
      0
      (+ (length (cdr items)))))
```

もうひとつの確立されたプログラミングテクニックは、リストをcdrダウンしながら答えのリストを"consアップ"するというものです。
これは、次のappend手続きで使われます。

* もしlist1がからリストであれが、結果はlist2である。
* そうでなければ、list1のcdrとlist2をappendし、その結果にlist1のcarをconsする。

```lisp
(define (append list1 list2)
  (if (null?  list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
```

### リストに対するマップ
非常に便利なえんざんとして　、リストのそれぞれの要素に何らかの変換を適用し、結果のリストを返すというものがあります。
例えば、次の手続きは、与えられた係数をリストのそれぞれの数値にかけます。
```lisp
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                         factor))))

(scale-list (list 1 2 3 4 5) 10)
```
このscale-listはさらに抽象化させた手続きmapに昇華させることが可能です。
```lisp
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
```
これを使用して、scale-listの新しいて技を書くことができます。

```lisp
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
```
mapは重要ながいねですが、それはmapが共通パターンを捉えているという理由からだけではありません。
mapを定義することで、scale-listの定義は**プロセスに焦点を当てた定義をすることが可能になっています。**これは、listからlistを返すプロセスに抽象化の壁を設けることをしたことで可能になります。

    
### 2.2.2 階層構造
list内にlistを定義することで階層構造を構築することが可能です。
これらに関して、葉の要素がいくつあるかをカウントするための関数は下記の様に定義します。

```lisp
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
```

