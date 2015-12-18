(define (square-list items)
  (cond ((null? items) (quote ()))
        (else
         (cons (square (car items)) (square-list (cdr items))))))

(define (square-list items)
  (map square items))

(define (square x)
  (* x x))
