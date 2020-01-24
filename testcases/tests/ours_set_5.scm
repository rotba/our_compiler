((lambda (a) 
    (set! a 5)
    (set! a 6)
    (set! a 7)
    (set! a 8)
    (set! a (if #t ((lambda (a b c)
                    ((lambda (d e f) (and a b c d e f)) 1 2 3)) 9 8 900) 700)) 
     a) 6)
; should return 800