(define x (lambda (x pred) (if pred x #f))) (define y (lambda () ((lambda () ((lambda () #t)))))) (begin 5 6 7 8 '(1 2 3) (x "PASS" (y)))