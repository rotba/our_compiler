(define fold-left
  (let ((null? null?)
	(car car) (cdr cdr)
	)
    (letrec ((fold-loop (lambda (f acc l)
		     (if (null? l)
			 acc
			 (fold-loop f (f acc (car l)) (cdr l))
			 ))))
      fold-loop
	)
   )
)

