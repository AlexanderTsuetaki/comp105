;; step 5

(check-type 3 int)
(check-type #t bool)
(check-type 'hello sym)

;; step 6

;; if bool then t1 else t2

(check-type (if #t 1 2) int)
(check-type (if #f 1 2) int)
(check-type (if #f '3t '4f) sym)
(check-type-error (if 'a 1 2))
(check-type-error (if #t '1a 2))

;; step 7
(check-type-error x)
(val y 5)
(check-type y int)
(val z 'a)
(check-type z sym)


;; step 9
;; VAL
(val a '3t)
(val a '4f)
;; EXP
(if #f 'exptestT 'exptestF)
(if #t 'exptestT 'exptestF)

;;step 11
(val n 0)
(check-type (let ([n, q])  n) int)


;; step 12
(check-type (lambda ([x : int]) x) (int -> int))

;; step 13
(val q 1)
(check-type (set q 15) int)

(check-type (while #t q) unit)

(check-type (begin q q) int)

;; step 14


;; step 15

;; step 16
(val-rec ["a" : (int -> int)] (lambda ([x : int]) (+ x x)))

(define int test_fun ([x : int]) 1)

;; step 17

(check-type (type-lambda ['a] (lambda ([f : 'a]) f)) (forall ['a] ('a -> 'a)))

;; step 18
