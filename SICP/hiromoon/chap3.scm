;paractice3-1

(define (make-accumulator n) 
  (lambda (m)
    (if (>= n m)
        (begin (set! n (+ n m))
               n)
        "Insufficient funds")))

;paractice3-2
(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          count
          (begin (set! count (+ count 1))
                 (f arg))))))

;paractice3-3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda _ "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;paractice3-4
(define (make-account balance password)
  (define count 0)
  (define (withdraw amount)
    (set! count 0)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! count 0)
    (set! balance (+ balance amount))
    balance)
  (define call-cops
    (lambda () (
    (if (>= count 7)
        "Called Cops!!!"
        (begin (set! count (+ count 1)) "Incorrect password"))))
  (define (dispatch p m)
    (cond ((not (eq? p password)) call-cops)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;paractice3-5
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-remaining 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (- x2 x1)
        (- y2 y1))
        (monte-carlo trials P)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (in-circle)
  (>= 1 (+ (square (random-in-range -1.0 1.0))
           (square (random-in-range -1.0 1.0)))))

(define (estimate-pi)
  (estimate-integral in-circle -1.0 1.0 -1.0 1.0 1000))

;paractice3-6
(define (r)
  (let ((seed 0))
    (define (dispatch arg)
      (cond ((eq? arg 'generate)
             (begin (set! seed (rand-update seed))
                    seed)) 
            ((eq? arg 'reset)
             (lambda (x) (set! seed x)))
            (else (error "invalid argument"))))
    dispatch))

;paractice3-7
(define (make-account balance password)
  (let ((passwords '(password)))
    ((define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (set-password p)
      (define (iter ps p)
        (if (null? ps)
            (set! passwords (append passwords p))
            (iter (cdr ps) p))
        iter))
    (define (verify-password p)
      (define (iter ps p)
        (if (null? ps) 
            #false
            (if (eq? (car ps) p)
                #true
                (iter (cdr ps) p))))
      (iter passwords p))
    (define (dispatch p m)
      (cond ((not (verify-password p) (lambda _ "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch)))

(define (make-joint account password new-password)
        ((account password 'set-password) new-password))

;paractice3-8
(define (f x)
  (let (num nil)
    (if (null? num)
        (begin
          (set! num x)
          num)
        num)))

;paractice3-9
;paractice3-10
;paractice3-11
; 図を書く問題なのでスキップ

;paractice3-12
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(cdr x) ; '(b)

(define w (append! x y))

(cdr x) ; '(b c d)

;paractice3-13
;循環参照になっているので無限ループになる

;practice3-14
;リストを逆順に並べ替える
;v: '()
;w: '(d c b a)
