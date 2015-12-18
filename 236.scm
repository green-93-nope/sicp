(define (accumulate op init seqs)
  (if (null? seqs)
      init
      (op (car seqs)
          (accumulate op init (cdr seqs)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (car-n seqs))
            (accumulate-n op init (cdr-n seqs)))))

(define (car-n seqs)
  (if (null? seqs)
      '()
      (cons (car (car seqs))
            (car-n (cdr seqs)))))

(define (cdr-n seqs)
  (if (null? seqs)
      '()
      (cons (cdr (car seqs))
            (cdr-n (cdr seqs)))))

(define (car-n seqs)
  (map car seqs))

(define (cdr-n seqs)
  (map cdr seqs))
