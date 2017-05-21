(load "312.scm")
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
