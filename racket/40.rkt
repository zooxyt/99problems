#lang racket

; P40 (**) Goldbach's conjecture.
; Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
; Example:
; * (goldbach 28)
; (5 23)

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

(define elem
  (lambda (e lst)
    (if (null? lst)
        #f
        (if (eqv? e (car lst))
            #t
            (elem e (cdr lst))))))

(define goldbach-try
  (lambda (sum try-list all-list)
    (let* ((x (car try-list))
           (y (- sum x)))
      (if (elem y all-list)
          (list x y)
          (goldbach-try sum (cdr try-list) all-list)))))

(define goldbach
  (lambda (x)
    (let ((primes (prime-range 2 x)))
      (goldbach-try x primes primes))))








