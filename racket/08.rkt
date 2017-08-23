#lang racket

; P08 (**) Eliminate consecutive duplicates of list elements.
; If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

; Example:
; * (compress '(a a a a b c c a a d e e e e))
; (A B C A D E)

(define lasts-helper
  (lambda (first rest)
    (if (null? rest)
        1
        (if (eqv? first (car rest))
            (+ 1 (lasts-helper first (cdr rest)))
            1))))
   
(define lasts
  (lambda (lst)
    (if (<= (length lst) 1)
        (length lst)
        (lasts-helper (car lst) (cdr lst)))))

(define drop
  (lambda (n lst)
    (if (= n 0)
        lst
        (drop (- n 1) (cdr lst)))))

(define compress
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((first (car lst))
              (count (lasts lst)))
          (cons first (compress (drop count lst)))))))
