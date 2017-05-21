(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (memq symbol (symbols tree)))
         '())
        ((not (memq symbol (symbols tree)))
         (error "bad symbol -- ENCODE-SYMBOL" symbol))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        (else
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(define (memq item list)
  (if (null? list)
      #f
      (or (eq? item (car list))
          (memq item (cdr list)))))
