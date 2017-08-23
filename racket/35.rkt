#lang racket

; P35 (**) Determine the prime factors of a given positive integer.
; Construct a flat list containing the prime factors in ascending order.
; Example:
; * (prime-factors 315)
; (3 3 5 7)

(define range
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (range (+ 1 low) high)))))

(define is-prime
  (lambda (x)
    (let ((r (range 2 (sqrt x))))
      (= (length (filter (lambda (y) y)
                         (map (lambda (y)
                                (= (modulo x y) 0))
                              r))) 0))))

(define prime-range
  (lambda (m n)
    (filter is-prime (range m n))))

(define factorize
  (lambda (x possible-primes result)
    (if (= x 1)
        result
        (let ((term-factors (filter (lambda (y)
                                           (= 0 (modulo x y)))
                                    possible-primes)))
          (factorize (quotient x (foldl * 1 term-factors))
                     possible-primes
                     (append term-factors result))))))
 
(define prime-factors
  (lambda (x)
    (let* ((primes (prime-range 2 x))
           (possible-primes (filter (lambda (y) (= (modulo x y) 0)) primes)))
      (factorize x possible-primes '()))))
