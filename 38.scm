(define f
  (let ((x 1))
    (lambda (new)
      (begin
        (set! x (* x new))
        x))))
