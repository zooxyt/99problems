#lang racket

; P11 (*) Modified run-length encoding.
; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

; Example:
; * (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

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

(define encode-modified
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((count (lasts lst)))
          (cons (if (= count 1)
                    (car lst)
                    (list count (car lst)))
                (encode-modified (drop count lst)))))))
