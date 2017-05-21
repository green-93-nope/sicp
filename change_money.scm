(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-change)
  (cond
   ((= amount 0) 1)
   ((or (< amount 0) (= kinds-of-change 0)) 0)
   (else
    (+ (cc amount (- kinds-of-change 1))
       (cc (- amount (value-of-kinds kinds-of-change))
           kinds-of-change)))))

(define (value-of-kinds kinds-of-change)
  (cond
   ((= kinds-of-change 1) 1)
   ((= kinds-of-change 2) 5)
   ((= kinds-of-change 3) 10)
   ((= kinds-of-change 4) 25)
   ((= kinds-of-change 5) 50)))
