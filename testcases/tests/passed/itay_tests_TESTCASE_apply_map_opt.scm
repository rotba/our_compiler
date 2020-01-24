(define (reverse-list l)
  (if (null? l)
     '()
     (append (reverse-list (cdr l)) (list (car l)))
  )
)

(define (reverse-list-of-lists lists)
	(map (lambda l (apply reverse-list l)) lists)
)

(reverse-list-of-lists '((1 2 3) (4 5 6) (7 8 9)))
