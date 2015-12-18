(define (number-approximate a b epsion)
  (< (abs (- a b)) epsion))

(define (same-key? key1 key2)
  (let ((epsion 0.001))
    (cond ((and (number? key1) (number? key2))
           (number-approximate key1 key2 epsion))
          ((or (number? key1) (number? key2))
           #f)
          (else
           (equal? key1 key2)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else
             (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else
             (error "Unknown operation -- TABLE" m))))
    dispatch))
(define test-table (make-table same-key?))
(define get-test (test-table 'lookup-proc))
(define put-test (test-table 'insert-proc!))
