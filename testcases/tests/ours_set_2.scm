((lambda (a) 
    (set! a 5)
    (set! a 6)
    (set! a 7)
    (set! a 8) 
     a) 6)
; should return 8