(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge sets)
  (cond ((null? sets) '())
        ((atom-in-list sets)
         (car sets))
        (else
         (successive-merge
          (adjoin-set (make-code-tree (car sets)
                                      (cadr sets))
                      (cddr sets))))))

(define (atom-in-list set)
  (and (pair? set) (null? (cdr set))))
