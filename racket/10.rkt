#lang racket

; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

; Example:
; * (encode '(a a a a b c c a a d e e e e))
; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

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

(define encode
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((count (lasts lst)))
          (cons (list count (car lst)) (encode (drop count lst)))))))
