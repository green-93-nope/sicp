(load "239.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (if (null? positions)
      #t
      (safe-check-iter (car positions) (cdr positions) 1)))

(define (safe-check-iter new rest i)
  (if (null? rest)
      #t
      (let ((first-check (car rest)))
        (if (or (= first-check new)
                (= (+ first-check i) new)
                (= (- first-check i) new))
            #f
            (safe-check-iter new (cdr rest) (+ i 1))))))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row
        rest-of-queens))
