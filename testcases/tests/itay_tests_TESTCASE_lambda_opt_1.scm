(define foo 
	(lambda (x y . s) 
		(lambda (a b . c) 
			(lambda (t) (+ x y) c))))
(((foo 1 2 3 4 5) "a" #t) #\b)
