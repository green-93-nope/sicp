#lang racket
(require racket/match)

(define (tree-sum exp)
  (match exp
    [(? number? x) x]
    [(list e1 e2)
       (let ([v1 (tree-sum e1)]
             [v2 (tree-sum e2)])
         (+ v1 v2))]))

(define r1-cal
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,op ,e1 ,e2)
       (let ([v1 (r1-cal e1)]
             [v2 (r1-cal e2)])
         (match op
           [`+ (+ v1 v2)]
           [`- (- v1 v2)]
           [`* (* v1 v2)]
           [`/ (/ v1 v2)]))])))

(define env0 '())

(define ext-env
  (lambda (x y env)
    (cons `(,x . ,y) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

(struct Closure (f env))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
           ))])))
