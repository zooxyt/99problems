#lang racket

; P22 (*) Create a list containing all integers within a given range.
; If first argument is smaller than second, produce a list in decreasing order.
; Example:
; * (range 4 9)
; (4 5 6 7 8 9)

(define range
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (range (+ 1 low) high)))))
