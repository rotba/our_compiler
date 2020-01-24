(define f5good
   (lambda (a b c d e) a))

(define id
    (lambda (x y z) x))

(apply id '(1 2 3))
(apply f5good 1 2 '(3 6 7))
(apply f5good 1 2 3 '(4 5))
(apply f5good 1 2 3 4 5 '())