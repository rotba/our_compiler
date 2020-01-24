(define 5!
    (letrec ((fact1 
            (lambda (n)
                (if (= n 0)
                1
                (* n (fact1 (- n 1)))))))
      (fact1 5)))

5!
