(define (lookup-variable-in-environment var env)
  (if (eq? env the-empty-environment)
      false
      (let ((result (lookup-variable-in-frame var (first-frame env))))
        (if (true? result)
            result
            (lookup-variable-in-frame var (enclosing-environment env))))))

(define (lookup-variable-in-frame var frame)
  (define (scan-in-frame vars vals)
    (cond ((null? vars) false)
          ((eq? var (car vars)) vals)
          (else (scan-in-frame (cdr vars) (cdr vals)))))
  (scan-in-frame (frame-variables frame) (frame-values frame)))

(define (lookup-variable-value var env)
  (let ((pos (lookup-variable-in-environment var env)))
    (if pos
        (car pos)
        (error "Unbound variable" var))))
(define (set-variable-value! var val env)
  (let ((pos (lookup-variable-in-environment var env)))
    (if pos
        (set-car! vals val)
        (error "Unbound variable --SET!" var))))

(define (define-variable! var val env)
  (let ((first (first-frame env))
        (pos (lookup-variable-in-frame var first)))
    (if pos
        (set-car! vals val)
        (add-binding-to-frame! var val first))))
