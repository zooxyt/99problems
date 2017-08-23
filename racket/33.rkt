#lang racket

; P33 (*) Determine whether two positive integer numbers are coprime.
; Two numbers are coprime if their greatest common divisor equals 1.
; Example:
; * (coprime 35 64)
; T

(define gcd
  (lambda (m n)
    (if (= n 0)
        m
        (gcd n (modulo m n)))))

(define coprime
  (lambda (m n)
    (= (gcd m n) 1)))
