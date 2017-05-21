(define tree-sum
  (lambda (exp)
    (let ((a (car exp))
          (b (cadr exp)))
      (letrec ((son-tree
                (lambda (x)
                  (if (pair? x)
                      (tree-sum x)
                      x))))
        (+ (son-tree a)
           (son-tree b))))))
(tree-sum '((1 (2 3)) (2 3)))
