(define (make-accumulator sum)
  (lambda (augend)
    (begin
      (set! sum (+ sum augend))
      sum)))
