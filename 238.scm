(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (op (fold-left op initial (cdr sequence))
          (car sequence))))
