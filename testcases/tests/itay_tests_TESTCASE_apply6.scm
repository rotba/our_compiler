(define plus +)
(define variadic-plus (lambda (init . args) (apply plus init args)))
(variadic-plus 1 2 3 4 5)
