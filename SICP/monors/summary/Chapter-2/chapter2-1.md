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
#### 木に対するマップ
木構造では、リスト内にリストを入れることが可能である。
これに対して、map処理をするには本来は、ループを書くことで対応をする。
しかし、mapと再帰を利用することで、多重ネストのListにも対応かのうである。

引数：数値の係数、歯が数艇である木
戻り値：同じ木
```lisp
(define (scala-tree tree factor)
  (cond (null? tree) nil)
        ((not (pari? tree)) (* tree factor))
        (else (cons (csale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))))

(scale-tree (list 1 (list 2 (3 4) 5) (list 6 7)) 10 )
```
scale-treeの別の実装方法として、木を部分木の列とみなしてmapを使うというものがあります。

```lisp
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (sale-tree sub-tree factor)
              (* sub-tree factor)))
        tree))
```

#### マップのネスト(mapのネスト)
mapのネストとしては、行列の内積を算出する際に一度使用済みである。
mapのネストはネストしたfor文を表すことが可能です。

"正の整数nが与えられた時1 <= i < j <= \nで、かつi+jが素数となる様な異なる正の整数iとjのすべての順序つきペアーを見つけよ"
という例があった場合、

求め方の戦略は下記の通りとなるだろう。
1. 1~nの整数を列挙する
2. 1の任意の数iに対してi < jを満たす様なペアの列を作成する。
3. 2に対してi+jが奇数であるペアのみに絞る。
4. 3.で算出されたものを(i, j, i+j)の形式で表示する

この時、1, 2はネストしたループとして表現される。これをmapとして表現することが可能である。

```lisp
(accumulate 
  append
  '()
  (map
    (lambda (i)
      (map 
        (lambda (j)
          (lambda i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
```
この様に、mapとaccumulateをappendによって組み合わせる事はこの種のプログラムではとてもよくある事ですので、独立した手続きとして分離することにします。


```lisp
(define (flatmap proc seq)
  (accumulate appenc '() (map prop seq)))
```
次に、このペアを列をフィルタして、我が奇数となるものを探します。

```lisp
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
```
最後にフィルとを通ったペアの列に対して次の手続きでマップして、結果の列を生成します。

```lisp
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ car pair) (cadr pair)))
```

これらをすべて組み合わせると、完全な手続きとなります。

```lisp
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))
```
mapのネストは、区間の列挙(enumerate-intervali)以外にも役立ちます。
例えば、集合{1 2 3}に対する順列を考えることりしましょう。
1, 2, 3のうち任意の１つを選択（X）しそれ以外の順列の先頭にXをつけるという方法が使えます.

```lisp
#lang racket
(define (flatmap proc seq)
  (append '() (map proc seq)))

(define (permutation s)
  (if (null? s)
      '()
      (flatmap
       (lambda (x)
              (map (lambda (p)
                     (cons x p))
                   (permutations (remove x s))))
            s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(permutation (list 1 2 3))
```
## 2.2.4: 例）図形言語
`cons`でpairからpairを作成できる。これは、表現ツールとしてリスト構造の持つ重要性の本質となる。性質であり**閉包性**といいます。一般にデータオブジェクトを組み合わせる操作は、その円座員による組み合わせの結果自身が同じ演算によって組み合わせる事が出来るのであれば、**閉包性を満たします。**

図形を描く簡単な言語を紹介し、それによって抽象と閉包の持つ力を示し、また高等な手続きの本質的な部分を利用します。


### 図形言語
painterという要素を今回使用して画像を操作していく。
ペインターはしていされた平行四辺形の枠にフィットするよういに、していされた画像をズラしたり、拡大したり、すたものを描画します。

画像を結合するには、与えられたペインタから新しいペインタを構築するいろいろな演算を使います。

* **beside**: 2つのペインターを引数にとり、枠の左半分に１つ目を、右側に２つ目のペインターを描画します。
* **below**: ２つのペインターを引数にとり、２つ目のペインターの画像の下に、１つめの画像を描く複合ペインタを作成します。
* **flip-vert**: 画像を上下逆に描くプリンターを作成します。
* **flip-horiz**: 左右逆に描くプリンタを作成します。

上記を組み合わせることで、複雑なパターンを作成する事が可能です。

### 高階演算
ペインタを組み合わせるパターンを抽象化するだけでなく、より高いレベルで、ペインタ演算を組み合わせるパターンを抽象化することもできます。
つまり、ペインタ演算を処理対象の要素として撮り、新しいペインタ演算を作る様な手続きを記述する事が可能です。

```lisp
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside tl painter) (tr painter))
          (bottom (beside (bl painter) (br painter))))
    (below bottom top))))
```

#### ペインタの変形と組み合わせ
ペインタに対する演算は、引数として受け取った**枠**から算出された**枠**に対して**元のペインタを呼び出す様なペインタ**を作る仕組みで動きます。

func(枠) : (枠の変換処理(元のペインタ）） -> 変換れたペインターが帰る。

```lisp
#lang racket
(require sicp-pict)

(define (transform-painter painter origin corner1 corner2)
  ;; 新しいFrameに対してpainterをmapingしているだけ。
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))
;;  origin * -> edge 1
;;         |
;;         v
;;         edge2

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0)))

(paint (flip-vert einstein))

;; 右上に1/4サイズに縮小して表示する。
(define (shrink-to-upper-right painter)
  (transform-painter
   painter
   (make-vect 0.5 0.5)
   (make-vect 1.0 0.5)
   (make-vect 0.5 1.0)))

(paint (shrink-to-upper-right einstein))

;; 枠を変換されたpainterはpainterから定義される。
```

#### 2.3.3 例：集合を表現する。
これまで有理数と代数式とういう２種類の複合データオブジェクトの構築は、データ構造が自明なものだった。

しかし、集合を考える場合そうは行かない。
可能な表現は数多くあり、それぞれいくつかの点でお互いに大きくとこなります。

ざっくりした言い方をすると、集合はただの異なるオブジェクトの集まりです。
この**集合**の定義をデータ抽象化で表した場合、**オブジェクトの操作**を実現する様な
複合データを構築するできればよいという逆説的な定義も可能です。

Ex)
下記操作を実現かのうならば、どのような表現を用いても良い。
* adjoin-set : オブジェクトと集合を引数に取り、もとの要素に加えて追加された要素を含む集合を返します。
* union-set : 二つの集合の和集合
* intersection-set : 二つの集合の共通部分

##### 順序なしリストとしての集合
集合を表現するやり方の一つは、そうそが二回以上会わられることのないリスト。
* 空集合: 空リストとして表現します。
* element-of-ser?: 集合内に特定の記号がリスト内に含まれていれば、リストの中で最初に見つかったサブリストを返す。含まれない場合は偽を返す。

##### 順序つきリストとしての集合
```lisp
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set) #t))
        ((< x (car set) #f))
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))
```
##### 2分木としての集合
集合の要素を木という形式で配置することで、順序つきリストとの表現をさらに改善できます。
binary-trueeにすることで、より効率てきに検索がを可能とする。

```listp
#lang racket
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (entry set) set) #t)
        (else (element-of-set? x
                              (if (> (entry set) set)
                                  (right-branch set)
                                  (left-branch set))))))
;; 
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


```

###### ハフマン木の表現
ハフマン木の表現はleafとtreeという２つの概念で実装可能です。
```lisp
#lang racket
(define (make-leaf sumbol weight) (list 'leaf symbol weight))
(deifne (leaf? object ( eq? (car object) 'leaf)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (maka-cond-tree left right)
  (list left
        right
        (appned (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbole tree)
  (if (leaf? tree)
      (list (symbole-leaf tree))
      (caddr tree))) ;; treeに紐づくtreeが再帰的にList化される。

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
```
手続きのsymbolesとweightは渡されたものが葉である場合と、一般の木である場合とで、異なる振る舞いをする必要があります。
これに関しては、ジェネリック手続き（２種類以上のデータを扱う手続き）で見ていきます。

##### 復号化手続き
複合化アルゴリズムを実装したものが下記です。
0と1のリストに加えてハフマン木を引数にとります。

bitを一つすすめ、要素がTreeの場合は次の要素に進む。
Leafの場合は、symbolの抽出を行う。
```lisp
(define (decode bits tress)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE_BRANCH" bit))))
    
```

###### 重みつき手続きの集合
二分木では、葉という要素はなく、すべての要素がSymbolと次の要素を持っていました。
そのため、単純なリスト構造で表すことが可能でした。
しかし、ハフマン木では、葉と木の概念を持っており、これらの２つの概念を区別することなく
小さい方から２つの項目を順にくっつける必要があります。

そのため、ハフマン木を作成する際に使用する、葉と木のリストは順序つきリストとして格納されていた方が都合が良いです。

```lisp
#lang racket
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) ( eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (maka-cond-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbole tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree))) ;; treeに紐づくtreeが再帰的にList化される。

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE_BRANCH" bit))))

;; 重みつきリストを順序リストとして表現する。
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; ((A 4) (B 2) (C 1) (D 1))のような記号と頻度のペアのリストをとり、初期状態での葉の順序つき集合を構築し、
;; ハフマンアルゴジズムにより、各種操作を行えるようにする。
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;> '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))
```

##### 2.4 中傷データの多重表現
プログラミングシステムは仕様変更を繰り返しながら、長期に渡って多くの人々によって設計されるということがよくあります。

この章では、プログラムの部品によって違う方法で表現されるデータを扱うやり方について学びます。
そのため、**ジェネリック手続き**（２種類以上の方法で表現されるデータを扱うための手続き)

ジェネリック手続きを構築するために使用するテクニック
* **タイプタグ**: どのように処理するかという情報を明示的に持っているデータオブジェクト
* **データ主導**: ジェネリック演算によって加法的にシステムを組み立てていくための協力で便利な実装戦略。

これらを導入することで、２つのデータ表現を用意しておき、後から入れ替えることを可能とする。

##### 例）複素数の表現
複素数の表現には２つのパターンがある。(直行形式と極形式）
これらのパターンには一長一短がありが、どちらのパターンで実装されていようと、
クライアントは実数の計算ができていればそれでよい。
これをデータの抽象化を使用して、定義する。

複素数の演算には、下記の４つのセレクタが必要になる。
**real-part**, **imag-part**, **magnitude**, **angle**である。

また複素数の２つのパターンは下記の通り表現可能。
```lisp
;; 直行形式
(make-from-real-imag (real-part z) (imag-part z))

;; 極形式
(make-from-mag-ang (magnitude z) (angle z))
```
次に、複素数の演算を実装する。
足し算と引き算は直行形式を用いて行い、掛け算と割り算は極形式を用いて行います。

```lisp
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-img (- (real-part z1) (real-part z2))
                      (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2)
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
```


