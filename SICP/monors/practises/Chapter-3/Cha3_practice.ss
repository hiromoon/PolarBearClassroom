;; 3.1
#lang racket
(define (make-accumulater init)
  (lambda (x)
    (set! init (+ x init))
    init)
  )

(define A (make-accumulater 5))

(A 10)
(A 10)

;; 3.2
#lang racket
(define (make-moniterd f)
  (let ((counter 0))
    (define show-count counter)
    (define (executer x)
      (begin
        (set! counter (+ counter 1))
        (f x)))
    (define (disptch m)
      (if (eq? m 'how-many-calls?)
          show-count
          (executer m)))
    disptch))

(define s (make-moniterd sqrt))
(s 100)
(s 'how-many-calls?)
