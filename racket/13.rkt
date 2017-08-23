#lang racket

; P13 (**) Run-length encoding of a list (direct solution).
;     Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

;     Example:
;     * (encode-direct '(a a a a b c c a a d e e e e))
;     ((4 A) B (2 C) (2 A) D (4 E))

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

(define encode-direct
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((count (lasts lst)))
          (cons (if (= count 1)
                    (car lst)
                    (list count (car lst)))
                (encode-direct (drop count lst)))))))
