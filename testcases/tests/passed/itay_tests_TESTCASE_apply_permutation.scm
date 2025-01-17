(define (insert l n e)
  (if (= 0 n)
      (cons e l)
      (cons (car l) 
            (insert (cdr l) (- n 1) e))))

(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))


(define (permute l)
  (if (null? l)
      '(())
      (apply append (map (lambda (p)
                           (map (lambda (n)
                                  (insert p n (car l)))
                                (seq 0 (length p))))
                         (permute (cdr l))))))

(permute '(1 2 3))
