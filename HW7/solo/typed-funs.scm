;;
;;excercise 9a
;;

; exists? (('a->BOOL) * (list a'-> BOOL))
; all? (('a ->BOOL) * (list a'-> BOOL))

;;
;;excercise 9b
;;

(val exists?
  (type-lambda ['a]
    (letrec [(  [exists-mono? : (('a -> bool) (list 'a) -> bool)]
        (lambda ([n : ('a -> bool)][ xs : (list 'a)])
          (if ([@ null? 'a] xs)
            #f
            (if (n ([@ car 'a] xs))
              #t
              (exists-mono? n ([@ cdr 'a] xs)))
            )
          )
        )] exists-mono?
      )
    )
  )
