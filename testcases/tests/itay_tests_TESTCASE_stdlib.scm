((lambda lst 
	(cons* lst 'factorial-of (letrec ((last
					(lambda (lst)
						(if (null? (cdr lst))
							(car lst)
							(last (cdr lst))))))
					(last lst))
		(make-string 3 #\=)
		(fold-right * 1
		   (fold-left (lambda (acc curr) 
				(append acc (list (car curr)))) '()
						(map list lst))))
) 1 2 3 4)

