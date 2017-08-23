#lang racket

; P18 (**) Extract a slice from a list.
; Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

; Example:
; * (slice '(a b c d e f g h i k) 3 7)
; (C D E F G)

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

(define slice
  (lambda (lst low high)
    (take (+ 1 (- high low)) (drop (- low 1) lst))))
