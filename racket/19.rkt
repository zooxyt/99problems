#lang racket

; P19 (**) Rotate a list N places to the left.
; Examples:
; * (rotate '(a b c d e f g h) 3)
; (D E F G H A B C)

; * (rotate '(a b c d e f g h) -2)
; (G H A B C D E F)

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

(define rotate
  (lambda (lst n)
    (if (>= n 0)
        (append (drop n lst)
                (take n lst))
        (append (drop (+ (length lst) n) lst)
                (take (+ (length lst) n) lst)))))
  
