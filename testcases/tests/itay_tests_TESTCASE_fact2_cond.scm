(define (factorial n)
 (cond ((< n 0) #f)
         ((or (= n 0) (= n 1)) 1)
         (else (* n (factorial (- n 1))))))

(factorial 12)
