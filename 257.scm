(load "ch232.scm")
(define (augend x)
  (let ((rest (cddr x)))
    (if (atom-in-list rest)
        (car rest)
        (cons '+ rest))))

(define (make-sum x . y)
  (let ((first-atom (car y)))
    (if (not (atom-in-list y))
        (cons '+
              (cons x y))
        (cond ((=number? x 0) first-atom)
              ((=number? first-atom 0) x)
              ((and (number? x) (number? first-atom)) (+ x first-atom))
              (else
               (list '+ x first-atom))))))

(define (multiplicand x)
  (let ((rest (cddr x)))
    (if (atom-in-list rest)
        (car rest)
        (cons '* rest))))

(define (make-product x y)
  (let ((first-atom (car y)))
    (if (not (atom-in-list y))
        (cons '*
              (cons x y))
        (cond ((or (=number? x 0) (=number? first-atom 0)) 0)
              ((=number? x 1) first-atom)
              ((=number? first-atom 1) x)
              ((and (number? x) (number? first-atom)) (* x first-atom))
              (else
               (list '* x first-atom))))))
