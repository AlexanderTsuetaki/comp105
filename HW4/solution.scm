;;
;;28
;;
;;B
(define Max* (xs)
  (if (null? xs)
    '()
    (foldr max
      (car xs)
      xs)
    )
  )
  (check-expect (Max* '()) '())
  (check-expect (Max* '(10 1 2 3 )) 10)
;;E
(define Sum (xs)
  (if (null? xs)
    '()
    (foldr +
      0
      xs)
    )
  )
  (check-expect (Sum '()) '())
  (check-expect (Sum '(10 1 2 3 )) 16)
;;F
(define Product (xs)
  (if (null? xs)
    '()
    (foldr *
      1
      xs)
    )
  )
  (check-expect (Product '()) '())
  (check-expect (Product '(10 1 2 3 )) 60)

;;
;;29
;;

;;a
(define append (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (foldr cons xs ys)
      )
    )
  )
  (check-expect (append '() '()) '())
  (check-expect (append '(10 1 2 3) '()) '(10 1 2 3))
  (check-expect (append '(10 1 2 3) '(4)) '(4 10 1 2 3))
;;c
(define reverse (xs)
  (foldl cons '() xs)
  )
  (check-expect (reverse '()) '())
  (check-expect (reverse '(10 1 2 3)) '(3 2 1 10))

;;
;;30
;;
;; map
(define even? (x) (= (mod x 2) 0))
(define map (f xs)
  (foldr (lambda (x y) (cons (f x) y ))
    '()
    xs
    )
  )
  (check-expect (map even? '()) '())
  (check-expect (map even? '(1 2 3)) '(#f #t #f) )

;; filter
(define filter (f xs)
  (foldr (lambda (x y) (if (f x) (cons x y) y))
    '()
    xs
    )
  )
  (check-expect (filter even? '()) '())
  (check-expect (filter even? '(1 2 3)) '(2) )
;; exists
(define exists? (f xs)
  (foldr (lambda (x y) (or (f x) y))
    #f
    xs
    )
  )
  (check-assert (not (exists? even? '())))
  (check-assert (exists? even? '(1 2 3)))
;; all
(define all (f xs)
  (foldr (lambda (x y) (and (f x) y))
    #t
    xs
    )
  )
  (check-expect (all even? '()) #t)
  (check-expect (all even? '(1 2 3)) #f )
  (check-expect (all even? '(2 4 6)) #t )
;;
;;38
;;

(val emptyset (lambda (x) #f))
(define member? (x s) (s x))

;;a
(val evens? (lambda (x) (= (mod x 2 ) 0 )))
(check-expect ( even? 1) #f)
(check-expect ( even? 2) #t)
;;b
(val two-digits? (lambda (x) (and (< x 100) (> x 9))))
(check-expect ( two-digits? 1) #f)
(check-expect ( two-digits? 22) #t)
;;c
(define add-element (x xs)
  (lambda (y) (or ( xs y) (= x y)))
  )
  (check-assert (member? 3 (add-element 3 even? )))
  (check-assert (member? 4 (add-element 3 even? )))
  (check-assert (not (member? 5 (add-element 3 even? ))))

(define union (xs ys)
  (lambda (y) (or (xs y) (ys y)))
  )
  (check-assert (member? 14 (union two-digits? evens?)))
  (check-assert (member? 99 (union two-digits? evens? )))
  (check-assert (member? 4 (union two-digits? evens? )))
  (check-assert (not (member? 5 (union two-digits? evens? ))))
(define intersect (xs ys)
  (lambda (y) (and (xs y) (ys y)))
  )
  (check-assert (not(member? 3 (intersect (lambda(y) (= 3 y)) evens?))))
  (check-assert (not (member? 99 (intersect two-digits? evens? ))))
  (check-assert (not (member? 4 (intersect two-digits? evens? ))))
  (check-assert (member? 54 (union two-digits? evens? )))
(define diff (xs ys)
  (lambda (y) (and (xs y) (not (ys y))))
  )
  (check-assert (not (member? 34 (diff two-digits? evens?))))
  (check-assert (member? 99 (diff two-digits? evens? )))
  (check-assert (not (member? 4 (diff two-digits? evens? ))))
  (check-assert (not (member? 54 (diff two-digits? evens? ))))
;;d
(record set-ops [ emptyset member? add-element union inter diff])
(define set-ops-from (eq?)
  (let ([emptyset  #f]
        [member? (lambda (x s) (exists? ((curry eq?) x) s ))]
        [add-element (lambda (y) (or ( xs y) (eq? x y)))]
        [union   (lambda (y) (or (xs y) (ys y)))]
        [inter (lambda (y) (and (xs y) (ys y)))]
        [diff (lambda (y) (and (xs y) (not (ys y))))])
      (make-set-ops emptyset member? add-element union inter diff)
    )
  )

  (check-assert (function? set-ops-from))
  (check-assert (set-ops? (set-ops-from =)))
  (val atom-set-ops (set-ops-from =))
  (val atom-set-emptyset (set-ops-emptyset atom-set-ops))
  (val atom-set-member? (set-ops-member? atom-set-ops))
  (val atom-set-add-element (set-ops-add-element atom-set-ops))
  (val atom-set-union (set-ops-union atom-set-ops))
  (val atom-set-inter (set-ops-inter atom-set-ops))
  (val atom-set-diff (set-ops-diff atom-set-ops))

;;
;;F
;;
;;   (flip (f)(lambda(_))  = '()
;;   (flip (f)(lambda(a))  = '()'
;;   (flip (f)(lambda(a b))  = (f b a)
(define flip (f)
  (lambda (x y) (f y x))
  )
(check-assert ((flip <) 3 2))
(check-assert ((flip >) 2 3))
(check-assert ((flip <=) 2 2))
(check-assert ((flip =) 2 2))
(check-assert (not ((flip <) 2 2)))
(check-assert (not ((flip <) 2 3)))
;;
;;O
;;
(define ordered-by? (f xs)
  (if (null? xs)
    #t
    (if (null? (cdr xs))
      #t
      (if (f (car xs) (cadr xs))
        (and #t (ordered-by? f (cdr xs)))
        #f
        )
      )
    )
  )
  (check-assert (ordered-by? < '(1 2 3)))
  (check-assert (not (ordered-by? <= '(1 3 2))))
  (check-assert (not (ordered-by? > '(1 2 3))))
  (check-assert (ordered-by? >= '(3 2 1)))
  (check-assert (ordered-by? = '(3 3 3)))
;;
;;V
;;
(val the_fault list1) ; build a singleton fault set
(val no_faults '()) ; empty fault set

; (faults/none response) == ’()
(define faults/none ()
    (lambda (y) no-faults))

; ((faults/always f) response) == (list1 f)
(define faults/always (f)
    (lambda (y) (the-fault f)))

; fill in algebraic laws and function implementation
(define faults/equal (key value)
    (lambda (y) (if (= (the-fault key) value) #t #f)))

; fill in algebraic laws and function implementation
(record faults/both [ member? add-element union intersect diff])
(val faults/both
    (let* ([member?  (lambda (x s) (exists? ((curry =) x) s))]
           [add-elem (lambda (x s) (if (member? x s) s (cons x s)))]
           [union  (lambda (faults1 faults2) (foldr add-elem faults2 faults1))]
           [intersect (lambda (faults1 faults2)
                              (foldr (if (and (member? faults1 faults2)
                                                (member? faults2 faults1))
                                                faults1 '() )))]
           [diff  (lambda (faults1 faults2)
                      (foldr (if (and (member? faults1 faults2)
                                      (member? faults2 faults1))
                                  '() (cons faults1 faults2))))]
                      )
          (make-faults/both member? add-element union intersect diff)
          )
      )

; fill in algebraic laws and function implementation
(define faults/switch (key validators)
  (validators[key]))
