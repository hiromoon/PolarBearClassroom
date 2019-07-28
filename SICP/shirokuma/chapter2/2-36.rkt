#lang racket

(define (accumulate-n op init sequence)
  (define nil '())
  (if (null? (car sequence))
      nil
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence))))) 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))) 