#lang racket

; P21 (*) Insert an element at a given position into a list.
; Example:
; * (insert-at 'alfa '(a b c d) 2)
; (A ALFA B C D)

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

(define insert-at
  (lambda (new-ele lst n)
    (append (take (- n 1) lst) (list new-ele) (drop (- n 1) lst))))
