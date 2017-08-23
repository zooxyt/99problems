#lang racket

; P25 (*) Generate a random permutation of the elements of a list.
; Example:
; * (rnd-permu '(a b c d e f))
; (B A D C E F)

; Hint: Use the solution of problem P23.

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

(define rnd-permu
  (lambda (lst)
    (let* ((len (length lst))
           (sels (lotto-select len len)))
      (map (lambda (x) (nth lst x)) sels))))

