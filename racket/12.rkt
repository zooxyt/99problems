#lang racket

; P12 (**) Decode a run-length encoded list.
; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(define repeat
  (lambda (n e)
    (if (= n 0)
        '()
        (cons e (repeat (- n 1) e)))))

(define decode-modified
  (lambda (lst)
    (if (null? lst)
        '()
        (let* ((cur (car lst))
               (rst (cdr lst))
               (cur-batch (if (pair? cur)
                              (let ((n (car cur))
                                    (e (cadr cur)))
                                (repeat n e))
                              (list cur))))
          (append cur-batch (decode-modified rst))))))

