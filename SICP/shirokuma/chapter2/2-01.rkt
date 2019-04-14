#lang racket

(define (add-rat x y)
  (make-rat (+  (* (numer x) (denom y))
                (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equals-rat x y)
  (= 
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

(define (numer n) (car n))

(define (denom d) (cdr d))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; test

(define one-halfA (make-rat 2 4))
(define one-halfB (make-rat 2 -4))
(define one-halfC(make-rat -2 4))
(define one-halfD (make-rat -2 -4))

(print-rat one-halfA)  ;  1/2
(print-rat one-halfB)  ; -1/2
(print-rat one-halfC)  ; -1/2
(print-rat one-halfD)  ;  1/2