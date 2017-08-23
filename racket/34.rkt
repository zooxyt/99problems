#lang racket

; P34 (**) Calculate Euler's totient function phi(m).
; Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
; Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

; * (totient-phi 10)
; 4

; Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).

(define range
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (range (+ 1 low) high)))))

(define gcd
  (lambda (m n)
    (if (= n 0)
        m
        (gcd n (modulo m n)))))

(define coprime
  (lambda (m n)
    (= (gcd m n) 1)))

(define totient-phi
  (lambda (x)
    (length (filter (lambda (y)
                      (coprime x y))
                    (range 1 (- x 1))))))
    
