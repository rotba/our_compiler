((lambda (a b)
    ((lambda (c) c) 5)) 6 7)

; should return 5
; tail call