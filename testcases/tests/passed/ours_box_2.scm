(((lambda (x y)
 (if x
    (lambda () (set! y x) y)
    (lambda (z) (set! x z) x))) #f 6) 9)

; should return 9