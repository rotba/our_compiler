(let (( f1 (lambda (x y) (+ x y)))
        ( f2 (lambda (z) (*  z z))))
   (f2(f1 (f1 (f1 1 2) 3) 4)))