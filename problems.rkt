#lang racket

(provide (all-defined-out))

(define (reverse-rec s)
  (cond [(null? s) s]
        [else
         (append (reverse-rec (cdr s)) (list (car s)))]))

(define (reverse-ite s)
  (define (reverse-acc r acc)
    (if (null? r)
        acc
        (reverse-acc (cdr r) (cons (car r) acc))))
  (reverse-acc s null))

(define (subset s)
  (if (null? s)
      (list '())
      (let ([rest (subset (cdr s))]
            [first (list (car s))])
        (append rest
                (map (lambda (x) (append x first)) rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (cond [(null? (car seqs)) null]
        [else
         (cons (accumulate op init (map car seqs))
               (accumulate-n op init (map cdr seqs)))]))

(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op (op initial (car sequence))
                 (cdr sequence))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (x) (map (lambda (y) (cons x y)) (enumerate-interval 1 (- x 1)))) (enumerate-interval 1 n)))

(define (tri-unique-pairs-s n s)
  (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
          (flatmap (lambda (x) (flatmap (lambda (y) (map (lambda (z) (list z y x)) (enumerate-interval 1 (- y 1)))) (enumerate-interval  1 (- x 1)))) (enumerate-interval 1 n))))


;;; This part is for mobile
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
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (branch-weight branch)
    (let ([st (branch-structure branch)])
      (cond [(integer? st) st]
            [(pair? st) (total-weight st)]
            [else (error "TOTAL-WEIGHT wrong structure" st)])))
(define (mobile-balance mobile)
  (define (branch-balance branch)
    (let ([st (branch-structure branch)])
      (cond [(integer? st) #t]
            [(pair? st) (mobile-balance st)]
            [else (error "MOBILE-BALANCE wrong structure" st)])))
  (let ([left (left-branch mobile)]
        [right (right-branch mobile)])
    (and (= (* (branch-length left) (branch-weight left))
            (* (branch-length right) (branch-weight right)))
         (and (branch-balance left) (branch-balance right)))))

;;; This part is for eight queens
(define (queens board-size)
  (define (queen-col k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (board) (safe? k board))
                (flatmap (lambda (positions)
                           (map (lambda (i)
                                  (adjoin-position i k positions))
                                (enumerate-interval 1 board-size)))
                         (queen-col (- k 1))))))
  (queen-col board-size))

(define empty-board null)
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? col positions)
  (let ([row (car positions)]
        [rest (cdr positions)])
    (define (check-iter rest d)
      (if (null? rest)
          #t
          (let ([cur (car rest)])
            (and (not (or (= cur row)
                          (= cur (+ row d))
                          (= cur (- row d))))
                 (check-iter (cdr rest) (+ d 1))))))
    (check-iter rest 1)))
