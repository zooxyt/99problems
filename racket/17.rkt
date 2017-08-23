#lang racket

; P17 (*) Split a list into two parts; the length of the first part is given.
; Do not use any predefined predicates.

; Example:
; * (split '(a b c d e f g h i k) 3)
; ( (A B C) (D E F G H I K))

(define drop
  (lambda (n lst)
    (if (= n 0)
        lst
        (drop (- n 1) (cdr lst)))))

(define take
  (lambda (n lst)
    (if (= n 0)
        '()
        (cons (car lst) (take (- n 1) (cdr lst))))))

(define split
  (lambda (lst n)
    (list (take n lst) (drop n lst))))
