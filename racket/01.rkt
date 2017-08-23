#lang racket

; P01 (*) Find the last box of a list.
; Example:
; * (my-last '(a b c d))
; (D)

(define my-last
  (lambda (lst)
    (if (eqv? (length lst) 1)
        (car lst)
        (my-last (cdr lst)))))
