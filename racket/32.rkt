#lang racket

; P32 (**) Determine the greatest common divisor of two positive integer numbers.
; Use Euclid's algorithm.
; Example:
; * (gcd 36 63)
; 9

(define gcd
  (lambda (m n)
    (if (= n 0)
        m
        (gcd n (modulo m n)))))
