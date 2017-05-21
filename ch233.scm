(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (let ((the-entry (entry set)))
    (cond ((null? set) #f)
          ((= x the-entry) #t)
          ((< x the-entry)
           (element-of-set? x (left-branch set)))
          (else
           (element-of-set? x (right-branch set))))))

(define (adjoin-set x set)
  (let ((the-entry (entry set)))
    (cond ((null? set) (make-tree x '() '()))
          ((= x the-entry) set)
          ((< x the-entry)
           (make-tree the-entry
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          (else
           (make-tree the-entry
                      (left-branch set)
                      (adjoin-set x (right-branch set)))))))
