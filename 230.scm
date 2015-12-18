(define (scale-tree tree factor)
  (map
   (lambda (x)
     (if (pair? x)
         (scale-tree x factor)
         (* x factor)))
   tree))

(define (square-tree tree)
  (map
   (lambda (x)
     (if (pair? x)
         (square-tree x)
         (* x x)))
   tree))
