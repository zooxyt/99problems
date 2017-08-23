#lang racket

; P39 (*) A list of prime numbers.
; Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

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

