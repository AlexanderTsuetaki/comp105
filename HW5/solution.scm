;********************
;*Alexander Tsuetaki*
;*Comp 105-HW5      *
;*Continuations     *
;********************
;;;;
;;
;;Problem L
;;
;;list-of?
;;   (list-of? x) for some atom x = (#f)
;;   (list-of? '()) = #t
;;   (list-of? '(f? x::xs)) = (and (f? x) list-of? xs)
;;   if xs is an atom and not a list or function we return false
;;   then if xs is null we return true as every empty list can be a list of
;;   some data type
;;   we then can use recursion to see if all values are accepted by a good f?
(define list-of? (f? xs)
    (if (null? xs)
        #t
        (if (atom? xs)
            #f
            (if (f? (car xs))
                (list-of? f? (cdr xs))
                #f
                 )
             )
        )
    )
(define value? (_) #t)
(define even? (x) (if (number? x) (if (= (mod x 2) 0) #t #f) #f))
    (check-assert (list-of? value? '()))
    (check-assert (list-of? value? '(1 2)))
    (check-assert (list-of? value? '(#t #f)))
    (check-assert (list-of? value? '(1 #t)))
    (check-assert (not(list-of? even? '(1 2))))
    (check-assert (list-of? even? '(4 2)))
    (check-assert (not(list-of? even? '(#t #f))))


(record not [arg])
(record or [args])
(record and [args])

;;
;;Problem F
;;
;;(formula? f) = #t , f is a symbol
;;(formula? (not? f)) = (list-of? formula? f)
;;(formula? (or? f))  = (list-of? formula? f)
;;(formula? (and? f)) = (list-of? formula? f)
;;(formula? (y)) = #f , y not a symbol or a record
(define formula? (f)
  (if (symbol? f)
    #t
    (if (not? f)
      (list-of? formula? (not-arg f))
      (if (or? f)
        (list-of? formula? (or-args f))
        (if (and? f)
          (list-of? formula? (and-args f))
          #f
          )
        )
      )
    )
  )

(check-assert (formula?(make-not '())))
(check-assert (formula?(make-or '())))
(check-assert (not (formula?(make-and (list3 1 2 3)))))
;;
;;Problem E
;;
;;(eval-formula f) = (foldr (if (= f env)) where f is a symbol
;;(eval-formula not?) = (not (eval-formula (not-arg f) env))
;;(eval-formula or?) = (foldr (or #f (eval-formula (or-args f) env)))
;;(eval-formula and?) = (foldr (and #t (eval-formula (and-args f) env)))

(define eval-formula (f env)
  (if (symbol? f)
    (find f env)
    (if (not? f)
      (not (eval-formula (not-arg f) env))
      (if (or? f)
        (foldr (lambda (a b) (or (eval-formula a env)  b )) #f (or-args f))
        (if (and? f)
          (foldr (lambda (a b) (and (eval-formula a env)  b )) #t (and-args f))
          #f
          )
        )
      )
    )
  )
(check-assert (eval-formula 'x '((x #t)(y #t)) ))
(check-assert (eval-formula 'y '((x #t)(y #t)) ))
(check-assert (eval-formula (make-or (list2 'x 'y)) '((x #t)(y #t)) ))
(check-assert (eval-formula (make-and (list2 'x 'y)) '((x #t)(y #t)) ))
(check-assert (eval-formula (make-not 'x) '((x #f)) ))
(check-assert (eval-formula (make-not (make-or (list2 'x 'y))) '((x #f)(y #f))))
(check-assert (eval-formula (make-not
                             (make-and (list2 'x 'y))) '((x #f)(y #f))))



;;
;;Problem S
;;


;;(find-formula-asst x bool curr fail succeed)
;;          == (find-formula-symbol x bool cur fail succeed)
;;(find-formula-asst (make-not f) bool curr fail succeed) ::bool
;;          == (find-formula-asst (cadr formula) (not bool) cur fail succeed)
;;(find-formula-asst (make-or f) bool curr fail succeed)  ::#t
;;          == (find-any-asst (cdr formula) bool cur fail succeed)
;;(find-formula-asst (make-or f) bool curr fail succeed)  ::#f
;;          == (find-all-asst (cdr formula) bool cur fail succeed)
;;(find-formula-asst (make-and f) bool curr fail succeed) ::#t
;;          == (find-all-asst (cdr formula) bool cur fail succeed)
;;(find-formula-asst (make-and f) bool curr fail succeed) ::#f
;;          == (find-any-asst (cdr formula) bool cur fail succeed)
;;(find-all-asst '() bool curr fail succeed)
;;          == fail
;;(find-all-asst (cons f fs) bool curr fail succeed)
;;          == (find-formula-asst (car formula) bool cur fail
;;  (lambda (cur resume) (find-all-asst (cdr formula) bool cur resume succeed)))
;;(find-any-asst '() bool curr fail succeed)
;;          == succeed
;;(find-any-asst (cons f fs) bool curr fail succeed)
;;          ==(find-formula-asst (car formula) bool cur fail
;;  (lambda (cur resume) (find-all-asst (cdr formula) bool cur resume succeed)))
;;(find-formula-symbol x bool curr fail succeed) where x is not bound in cur
;;          ==(succeed (bind formula bool cur) fail)
;;(find-formula-symbol x bool curr fail succeed) where x is bool in cur
;;          ==(succeed cur fail)
;;(find-formula-symbol x bool curr fail succeed) where x is (not bool) in cur
;;          ==(fail)
(define find-formula-true-asst (formula fail succeed)
  (letrec
         ((find-formula-asst
           (lambda (formula bool cur fail succeed)
              (if (atom? formula)
                (find-formula-symbol  formula bool cur fail succeed)
                (if (not? formula)
                    (find-formula-asst (not-arg formula)
                      (not bool) cur fail succeed)
                    (if (or? formula)
                        (if (= #t bool)
                            (find-any-asst (or-args formula)
                              bool cur fail succeed)
                            (find-all-asst (or-args formula)
                              bool cur fail succeed)
                            )
                        (if (and? formula)
                            (if (= #t bool)
                                (find-all-asst (and-args formula)
                                  bool cur fail succeed)
                                (find-any-asst (and-args formula)
                                  bool cur fail succeed)
                                )
                            (find-formula-asst (car formula)
                                bool cur fail
                                 (lambda (cur resume)
                                  (find-all-asst (cdr formula)
                                    bool cur resume succeed)))
                            )
                        )
                    )
                )
            ))
           (find-any-asst
             (lambda (formula bool cur fail succeed)
              (if (null? formula)
                  (fail)
                  (find-formula-asst formula bool cur
                                     (lambda ()
                                       (find-any-asst (cdr formula)
                                        bool cur fail succeed)) succeed)
                  )
              ))
           (find-all-asst
             (lambda (formula bool cur fail succeed)
              (if (null? formula)
                  (succeed cur fail)
                  (find-formula-asst (car formula) bool cur fail
                                     (lambda (cur resume)
                                       (find-all-asst (cdr formula)
                                        bool cur resume succeed)))
                  )
              ))
           (find-formula-symbol
             (lambda (formula bool cur fail succeed)
               (if (null? (find formula cur))
                   (succeed (bind formula bool cur) fail)
                   (if (= (find formula cur) bool)
                       (succeed cur fail)
                       (fail)
                       )
                   )
              ))
            )
    (find-formula-asst formula #t '() fail succeed))
    )
(check-assert (function? find-formula-true-asst))    ; correct name
(check-error (find-formula-true-asst))                ; not 0 arguments
(check-error (find-formula-true-asst 'x))             ; not 1 argument
(check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
(check-error
   (find-formula-true-asst 'x (lambda () 'fail) (lambda (c r) 'succeed) 'z))

(check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda () 'succeed)))
    ; success continuation expects 2 arguments, not 0
(check-error (find-formula-true-asst 'x (lambda () 'fail)
                                        (lambda (_) 'succeed)))
    ; success continuation expects 2 arguments, not 1
(check-error (find-formula-true-asst
                   (make-and (list2 'x (make-not 'x)))
                   (lambda (_) 'fail)
                   (lambda (_) 'succeed)))
    ; failure continuation expects 0 arguments, not 1


(check-expect   ; x can be solved
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'succeed)

(check-expect   ; x is solved by '((x #t))
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
   #t)

(check-expect   ; (make-not 'x) can be solved
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'succeed)

(check-expect   ; (make-not 'x) is solved by '((x #f))
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
   #f)

(check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
   (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'fail)
(check-expect   ; (make-and (list2 'x (make-not 'y))) should be '(#t #f)
  (find-formula-true-asst (make-and (list2 'x (make-not 'y)))
                          (lambda () 'fail)
                          (lambda (cur resume)
                                (list2 (find 'x cur )(find 'y cur))))
    '(#t #f))
(check-expect
  (find-formula-true-asst (make-or (list2 'x (make-and (list2 'x 'y))))
                          (lambda () 'fail)
                          (lambda (cur resume)
                                (list2 (find 'x cur )(find 'y cur))))
    '(#t #t))
(check-expect
  (find-formula-true-asst (make-or (list2 (make-and (list2 'x 'y)) 'x))
                          (lambda () 'fail)
                          (lambda (cur resume)
                                (list2 (find 'x cur )(find 'y cur))))
    '(#t #t))
