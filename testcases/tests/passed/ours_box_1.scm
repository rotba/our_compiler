(((lambda (x y)
 (if x
    (lambda () (set! y x) y)
    (lambda (z) (set! x z) x))) 5 6))

; should return 5