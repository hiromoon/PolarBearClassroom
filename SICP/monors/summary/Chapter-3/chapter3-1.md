# はじめに
モジュール化の考え方には２つの種類がある。
* オブジェクトに集中する... 状態は時間とともに変化する。
* システムに流れる情報のストリームに集中するもの

# 3.1 代入と局所状態
## 3.1.1. local状態変数
`set! <name> <new-value>`で値の更新が可能。
`begin <exp1> <exp2> <exp3>...`で順次評価をし、最後のexpの出力を行う。

```lisp
#lang racket
(define balance 1000)
(define (withdrow amount)
  (if (> balance 0)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficent fund"))

(withdrow 250)
```
しかし、balaceがグローバル変数になっているので、プライベート化するために下記のように定義し直します。

```lisp
#lang racket
(define new-withdraw
  (let (( balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount ))
                 balance)
          "Insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
```
また、下記のように記述することで**メッセージパッシング**を使用して、メソッドを呼び出せるようになる。
```lisp
#lang racket
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount ))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount ))
    balance)
  
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  
  dispatch)

(define acc (make-account 1000))
((acc 'deposit) 100)
```

### 3.3. 可変オブジェクトによるモデル化
可変オブジェクトを用いて、構造体を作成していくととする。
構造体の構築には、2章使用していたセレクタとコンストラクタに加えて、**ミューテーター**を定義しする。
例えば、口座の残高変更の場合、下記の呼び出しが可能なことがきたいされます。
`(set-balance! <account> <new-value>)`

ミューテターが定義されているオブジェクトは、**可変データオブジェクト**と言われます。
