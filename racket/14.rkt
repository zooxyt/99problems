#lang racket

; P14 (*) Duplicate the elements of a list.
; Example:
; * (dupli '(a b c c d))
; (A A B B C C C C D D)

(define dupli
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (cons (car lst)
                    (dupli (cdr lst)))))))
  
