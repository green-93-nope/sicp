(load "326_compare.scm")
(define (make-tree key value left-branch right-branch)
  (list key value left-branch right-branch))
(define (make-new-tree key value)
  (make-tree key value '() '()))
(define (value-of-tree tree)
  (cadr tree))
(define (key-of-tree tree)
  (car tree))
(define (left-branch tree)
  (caddr tree))
(define (right-branch tree)
  (cadddr tree))
(define (set-key! tree item)
  (set-car! tree item))
(define (set-value! tree item)
  (set-car! (cdr tree) item))
(define (set-left-branch! tree new-tree)
  (set-car! (cddr tree) new-tree))
(define (set-right-branch! tree new-tree)
  (set-car! (cdddr tree) new-tree))
(define (empty-tree? tree)
  (null? tree))
(define (lookup-tree tree key)
  (if (empty-tree? tree)
      #f
      (let ((compare-flag (equal-all? key (key-of-tree tree))))
        (cond
         ((= compare-flag 0) tree)
         ((= compare-flag 1) (lookup-tree (right-branch tree) key))
         ((= compare-flag -1) (lookup-tree (left-branch tree) key))))))
(define (insert-new-member tree key value)
  (if (empty-tree? tree)
      (make-new-tree key value)
      (let ((compare-flag (equal-all? key (key-of-tree tree))))
        (cond
         ((= compare-flag 0)
          (set-value! tree value)
          tree)
         ((= compare-flag 1)
          (set-right-branch! tree
                             (insert-new-member (right-branch tree) key value))
          tree)
         ((= compare-flag -1)
          (set-left-branch! tree
                            (insert-new-member (left-branch tree) key value))
          tree)))))

(define (make-tree-table)
  (let ((t '()))
    (define (lookup key)
      (let ((find-tree (lookup-tree t key)))
        (if find-tree
            (value-of-tree find-tree)
            #f)))
    (define (insert! key value)
      (if (null? t)
          (set! t (make-new-tree key value)))
      (insert-new-member t key value))
    (define (dispatch m)
      (cond
       ((eq? m 'lookup-proc) lookup)
       ((eq? m 'insert-proc) insert!)))
    dispatch))

(define tree-table (make-tree-table))
(define get-tree-table (tree-table 'lookup-proc))
(define put-tree-table (tree-table 'insert-proc))
