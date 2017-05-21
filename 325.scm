(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else
         (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (look-up-iter keys current-table)
        (if (null? keys)
            #f
            (let ((find-table (assoc (car keys) (cdr current-table))))
              (display find-table)
              (newline)
              (cond
               ((and (exist-records find-table) (null? (cdr keys)))
                (cdr find-table))
               (else
                (if find-table
                    (look-up-iter (cdr keys) find-table)
                    #f))))))
      (look-up-iter keys local-table))
    (define (insert! keys value)
      (define (insert-iter! keys value current-table)
        (if (null? keys)
            (error "ERROR INSERT! no key in the keys list" keys)
            (let ((find-table (assoc (car keys) (cdr current-table))))
              (cond
               ((null? (cdr keys))
                (if find-table
                    (set-cdr! find-table value)
                    (set-cdr! find-table (cons (cons (car keys) value)
                                               (cdr find-table)))))
               (else
                (if find-table
                    (insert-iter! (cdr keys) value (cdr current-table))
                    (set-cdr! current-table
                              (cons (make-key-table keys value)
                                    (cdr current-table)))))))))
      (insert-iter! keys value local-table))
    (define (make-key-table keys value)
      (cond ((null? keys)
             (error "ERROR MAKE-KEY-TABLE no key in the keys list" keys))
            ((null? (cdr keys))
             (cons (car keys) value))
            (else
             (list (car keys) (make-key-table (cdr keys) value)))))
    (define (exist-records find-table)
      (not (eq? find-table #f)))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else
             (error "Unknown operation -- TABLE" m))))
    dispatch))
(define many-keys-table (make-table))
(define get-many-keys-table (many-keys-table 'lookup-proc))
(define put-many-keys-table (many-keys-table 'insert-proc))
