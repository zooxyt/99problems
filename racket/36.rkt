#lang racket

; P36 (**) Determine the prime factors of a given positive integer (2).
; Construct a list containing the prime factors and their multiplicity.
; Example:
; * (prime-factors-mult 315)
; ((3 2) (5 1) (7 1))
; Hint: The problem is similar to problem P13.

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

(define lasts-helper
  (lambda (first rest)
    (if (null? rest)
        1
        (if (eqv? first (car rest))
            (+ 1 (lasts-helper first (cdr rest)))
            1))))
   
(define lasts
  (lambda (lst)
    (if (<= (length lst) 1)
        (length lst)
        (lasts-helper (car lst) (cdr lst)))))

(define drop
  (lambda (n lst)
    (if (= n 0)
        lst
        (drop (- n 1) (cdr lst)))))

(define encode
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((count (lasts lst)))
          (cons (list count (car lst)) (encode (drop count lst)))))))

(define prime-factors-mult
  (lambda (x)
    (encode (sort (prime-factors x) <))))

    
