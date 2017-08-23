#lang racket

; P09 (**) Pack consecutive duplicates of list elements into sublists.
;     If a list contains repeated elements they should be placed in separate sublists.

;     Example:
;     * (pack '(a a a a b c c a a d e e e e))
;     ((A A A A) (B) (C C) (A A) (D) (E E E E))


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

(define take
  (lambda (n lst)
    (if (= n 0)
        '()
        (cons (car lst) (take (- n 1) (cdr lst))))))
         
(define pack
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((count (lasts lst)))
          (cons (take count lst) (pack (drop count lst)))))))
