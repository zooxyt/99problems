#lang racket

;; P04 (*) Find the number of elements of a list.

(define number-of-elements
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (number-of-elements (cdr lst))))))

    
