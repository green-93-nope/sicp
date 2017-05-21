(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (mobile-in-structure? structure)
  (pair? structure))


(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile-in-structure? structure)
        (total-weight structure)
        structure)))

(define (mobile-balance? mobile)
  (letrec ((left (left-branch mobile))
           (right (right-branch mobile))
           (not-unequal?
            (lambda (structure)
              (if (mobile-in-structure? structure)
                  (mobile-balance? structure)
                  #t)))
           (branch-torque
            (lambda (branch)
              (* (branch-length branch)
                 (branch-weight branch)))))
    (if (and (not-unequal? (branch-structure left))
             (not-unequal? (branch-structure right)))
        (= (branch-torque left)
           (branch-torque right))
        #f)))
