(define z 10)
((lambda (x y) 
	(set! y 2)
	(+ x y z)) 42 43)