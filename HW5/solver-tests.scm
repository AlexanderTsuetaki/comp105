
;; not case determined at start
(val f1 (make-not 'x))
(val s1 '((x #f)))
;; tests  'and' and 'or' cases while also testing recursion in the first and
;;                       second positions (car and cdr positions)
(val f2 (make-and (make-and (list2 'x 'y')) (make-or (list2 'y (make-not 'x)))))
(val s1 '((x #t) (y #t)))
;; tests a fail case with no solution
(val f3 (make-and 'x (make-not 'x)))
(val s3 'fail)
