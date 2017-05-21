#lang racket

(provide (all-defined-out))

(define (make-table) (mcons '*table* null))

(define (lookup key table)
  (let ([record (assoc key (mcdr table))])
    (if record
        (mcdr record)
        #f)))

(define (assoc key records)
  (cond [(null? records) #f]
        [(equal? key (mcar (car records))) (car records)]
        [else
         (assoc key (cdr records))]))

(define (insert! key value table)
  (let ([record (assoc key (mcdr table))])
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (cons (mcons key value) (mcdr table))))))

(define (make-table2) (mcons '*doubletable* null))

(define (lookup2 key1 key2 table)
  (let ([subtable (assoc key1 (mcdr table))])
    (if subtable
        (let ([subrecord (assoc key2 (mcdr subtable))])
          (if subrecord
              (mcdr subrecord)
              #f))
        #f)))

(define (insert2! key1 key2 value table)
  (let ([record1 (assoc key1 (mcdr table))])
    (if record1
        (let ([record2 (assoc key2 (mcdr record1))])
          (if record2
              (set-mcdr! record2 value)
              (set-mcdr! record1 (cons (mcons key2 value) (mcdr record1)))))
        (set-mcdr! table
                   (cons (mcons key1
                                (cons (mcons key2 value) null))
                         (mcdr table))))))
