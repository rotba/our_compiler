(define makenList 
    (lambda (n)
        (if (= n 0)
            '()
            (cons n (makenList (- n 1)) ))
    )
)

(makenList 100)