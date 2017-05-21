(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
         (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (union-set-iter (car set1) (append (cdr set1) set2) '()))

(define (union-set-iter x set result)
  (cond ((null? set) (cons x result))
        ((element-of-set? x set)
         (union-set-iter (car set) (cdr set) result))
        (else
         (union-set-iter (car set) (cdr set) (cons x result)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((and (not (element-of-set? (car set1) (cdr set1)))
              (element-of-set? (car set1) set2))
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))
