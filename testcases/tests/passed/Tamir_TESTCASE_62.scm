(define x 0)
((lambda (x y) 
	(set! y 2)
	(+ x y)) 42 43)