#lang racket

; P31 (**) Determine whether a given integer number is prime.
; Example:
; * (is-prime 7)
; T

(define is-prime
  (lambda (x)
    (letrec ((range
              (lambda (low high)
                (if (> low high)
                    '()
                    (cons low (range (+ 1 low) high)))))
             (r (range 2 (sqrt x))))
      (= (length (filter (lambda (y) y)
                         (map (lambda (y)
                                (= (modulo x y) 0))
                              r))) 0))))



