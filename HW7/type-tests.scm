

(check-type (@ cdr int) ((list int) -> (list int)))
(check-type-error (cons 1 bool))
(check-type-error (lambda ([x : bool]) (+ x x)))
