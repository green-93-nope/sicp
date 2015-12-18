(define (union-set tree1 tree2)
  (list->tree
   (union-tree
    (tree->list2 tree1)
    (tree->list2 tree2))))

(define (intersection-set tree1 tree2)
  (list->tree
   (intersection-set
    (tree->list2 tree1)
    (tree->list2 tree2))))
