#lang racket

; P02 (*) Find the last but one box of a list.
; Example:
; * (my-but-last '(a b c d))
; (C D)

(define my-but-last
  (lambda (lst)
    (if (eqv? (length lst) 2)
        lst
        (my-but-last (cdr lst)))))
