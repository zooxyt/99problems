#lang racket

; P06 (*) Find out whether a list is a palindrome.
; A palindrome can be read forward or backward; e.g. (x a m a x).

(define reverse
  (lambda (lst)
    (if (null? lst)
        '()
        (append (reverse (cdr lst))
                (list (car lst))))))

(define palindorome?
  (lambda (lst)
    (if (<= (length lst) 1)
        #t
        (let ((the-first (car lst))
              (the-final (car (reverse lst))))
          (if (not (eqv? the-first the-final))
              #f
              (palindorome? (cdr (reverse (cdr (reverse lst))))))))))
