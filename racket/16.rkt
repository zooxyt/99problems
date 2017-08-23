#lang racket

; P16 (**) Drop every N'th element from a list.
;     Example:
;     * (drop '(a b c d e f g h i k) 3)
;     (A B D E G H K)

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

(define drop-every
  (lambda (lst n)
    (if (< (length lst) n)
        lst
        (append (take (- n 1) lst)
                (drop-every (drop n lst) n)))))
