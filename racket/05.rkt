#lang racket

;; P05 (*) Reverse a list.

(define reverse
  (lambda (lst)
    (if (null? lst)
        '()
        (append (reverse (cdr lst))
                (list (car lst))))))
