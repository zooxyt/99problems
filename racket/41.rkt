#lang racket

; P41 (**) A list of Goldbach compositions.
; Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
; Example:
; * (goldbach-list 9 20)
; 10 = 3 + 7
; 12 = 5 + 7
; 14 = 3 + 11
; 16 = 3 + 13
; 18 = 5 + 13
; 20 = 3 + 17

; In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

; Example (for a print limit of 50):
; * (goldbach-list 1 2000 50)
; 992 = 73 + 919
; 1382 = 61 + 1321
; 1856 = 67 + 1789
; 1928 = 61 + 1867

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

(define goldbach-list-try
  (lambda (low high result)
    (if (> low high)
        result
        (goldbach-list-try (+ 2 low) high (append result (list (goldbach low)))))))

(define goldbach-list
  (lambda (low high)
    (let ((result (goldbach-list-try (if (even? low) low (+ low 1))
                                     (if (even? high) high (- high 1))
                                     '())))
      (foldl (lambda (a b)
               (string-append b "\n" a))
             ""
             (map (lambda (item)
                    (let* ((x (car item))
                           (y (cadr item))
                           (sum (+ x y)))
                      (format "~a = ~a + ~a" sum x y))) result)))))
  
