#lang racket

; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
; The selected numbers shall be returned in a list.
; Example:
; * (lotto-select 6 49)
; (23 1 17 33 21 37)

; Hint: Combine the solutions of problems P22 and P23.

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

(define lotto-select
  (lambda (n m)
    (selectors-helper m n '())))
