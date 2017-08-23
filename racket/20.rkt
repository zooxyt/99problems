#lang racket

; P20 (*) Remove the K'th element from a list.
; Example:
; * (remove-at '(a b c d) 2)
; (A C D)

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

(define remove-at
  (lambda (lst n)
    (append (take (- n 1) lst) (drop n lst))))
