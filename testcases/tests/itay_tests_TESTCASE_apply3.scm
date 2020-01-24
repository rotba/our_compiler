(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(apply and-l #t #t #t #t '(#t #t #f))
