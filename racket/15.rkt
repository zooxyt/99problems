#lang racket

; P15 (**) Replicate the elements of a list a given number of times.
; Example:
; * (repli '(a b c) 3)
; (A A A B B B C C C)

(define repeat
  (lambda (n e)
    (if (= n 0)
        '()
        (cons e (repeat (- n 1) e)))))

(define repli
  (lambda (lst n)
    (if (null? lst)
        '()
        (append (repeat n (car lst))
                (repli (cdr lst) n)))))
        
