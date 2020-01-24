(define (length-tail list current-length)
  (if (null? list)
      current-length
      (length-tail (cdr list) ; recursive call, IS a tail-call
                (+ 1 current-length))))

(length-tail '(1 2 3 4 5 6) 0)
