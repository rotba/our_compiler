(define max-of-list (lambda (lst)
	(letrec ((max (lambda (m lst)
			(if (null? lst)
				m
				(if (> (car lst) m)
					(max (car lst) (cdr lst))
					(max m (cdr lst)))))))
		(max 0 lst))))

(max-of-list '(4 5 9 2 10 4 7 23 11 3 12 9 1 -4 9 15))
				
