#lang racket

; P23 (**) Extract a given number of randomly selected elements from a list.
; The selected items shall be returned in a list.
; Example:
; * (rnd-select '(a b c d e f g h) 3)
; (E D A)

; Hint: Use the built-in random number generator and the result of problem P20.

(define random-gen (make-pseudo-random-generator))

;; If an element occurs in a list
(define elem
  (lambda (e lst)
    (if (null? lst)
        #f
        (if (eqv? e (car lst))
            #t
            (elem e (cdr lst))))))

;; Pick the nth element
(define nth
  (lambda (lst n)
    (if (= n 0)
        (car lst)
        (nth (cdr lst) (- n 1)))))

;; Select n elements from m elements
(define selectors-helper
  (lambda (m n result)
    (if (= n 0)
        result
        (let ((idx (random m random-gen)))
          (if (elem idx result)
              (selectors-helper m n result)
              (selectors-helper m (- n 1) (cons idx result)))))))

(define selectors
  (lambda (m n)
    (selectors-helper m n '())))

(define rnd-select
  (lambda (lst n)
    (let ((sels (selectors (length lst) n)))
      (map (lambda (idx)
             (nth lst idx)) sels))))
             
