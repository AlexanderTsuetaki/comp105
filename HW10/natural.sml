(* inconvenient code *)
structure Natural :> NATURAL
  =
struct
(***********from online solutions**********)
   datatype nat = ZERO
                | TIMES10PLUS of nat * int
   exception Negative
   exception BadDivisor
   exception NotCompleted

   fun times10plus (ZERO, 0) = ZERO
    | times10plus (n, d) = TIMES10PLUS (n, d)
   (* times10 : nat -> nat *)
   fun times10 n = times10plus (n, 0)
   (* natOfDigit : int -> nat *)
   fun natOfDigit d = times10plus (ZERO, d)
   fun flip f (x, y) = f (y, x)
   (* natOfDigits : int list -> nat *)
   fun natOfDigits ds = foldl (flip times10plus) ZERO ds
   fun intOfNat ZERO = 0
    | intOfNat (TIMES10PLUS (m, d)) = 10 * intOfNat m + d
   fun natOfInt 0 = ZERO
    | natOfInt n = times10plus (natOfInt (n div 10), n mod 10)
   fun natString ZERO = "0"
    | natString (TIMES10PLUS (ZERO, d)) = Int.toString d
    | natString (TIMES10PLUS (n, d)) = natString n ^ Int.toString d
   fun carryIntoNat (n, 0) = n
    | carryIntoNat (ZERO, 1) = natOfDigit 1
    | carryIntoNat (TIMES10PLUS (m, d), 1) =
      times10plus (carryIntoNat (m, (d + 1) div 10), (d + 1) mod 10)
    | carryIntoNat _ = raise Match (* bad carry bit *)
   fun addWithCarry ( n1, ZERO, c) = carryIntoNat (n1, c)
    | addWithCarry (ZERO, n2, c) = carryIntoNat (n2, c)
    | addWithCarry (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2), c) =
      let val d = (d1 + d2 + c) mod 10
          val c = (d1 + d2 + c) div 10 (* the "carry out" *)
            in times10plus (addWithCarry (m1, m2, c), d)
          end
    fun addNats (n1, n2) = addWithCarry (n1, n2, 0)
    exception Negative
    fun borrowFromNat (n, 0) = n
     | borrowFromNat (TIMES10PLUS (m, 0), 1) = times10plus(borrowFromNat
                                                                  (m, 1), 9)
     | borrowFromNat (TIMES10PLUS (m, d), 1) = times10plus(m, d - 1)
     | borrowFromNat (ZERO, 1) = raise Negative
     | borrowFromNat _ = raise Match (* bad borrow bit *)
    fun subWithBorrow (n1, ZERO, b) = borrowFromNat (n1, b)
     | subWithBorrow (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2), b) =
     let val d = (d1 - d2 - b) mod 10
     val b = if d1 - d2 - b < 0 then 1 else 0
     in times10plus (subWithBorrow (m1, m2, b), d)
     end
     | subWithBorrow (ZERO, TIMES10PLUS _, b) =
     raise Negative (* works only because of invariant *)
    fun subNats (n1, n2) = subWithBorrow (n1, n2, 0)
    (* extra credit: multiplication *)
    fun mulNats (ZERO, _) = ZERO
     | mulNats (_, ZERO) = ZERO
     | mulNats (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2)) =
     addNats (natOfInt (d1 * d2),
     addNats(times10 (addNats(mulNats(m1, natOfDigit d2),
     mulNats(m2, natOfDigit d1))),
     times10 (times10 (mulNats (m1, m2)))))

(*********** HW10 work **********)
   fun digit (ZERO) = 0
    |  digit (TIMES10PLUS (n, d)) = d

   fun getNat (ZERO) = ZERO
    |  getNat (TIMES10PLUS (n, d)) = n

   fun ofInt (n) = natOfInt n
   fun /+/ (n1, n2) = addNats(n1, n2)
   fun /-/ (n1, n2) = subNats(n1, n2)
   fun /*/ (n1, n2) = mulNats(n1, n2)


    fun fst (i, _) = i
    fun snd (_, j) = j

   fun sdivmod (ZERO, i)= (ZERO, 0)
    | sdivmod (TIMES10PLUS  (n, d), i)=
       let
          val (m,l) = sdivmod (n, i)
       in
          (addNats(mulNats (m, natOfInt 10), natOfInt (((l * 10) + d) div i)),
          (l * 10 + d) mod i)
       end

   fun sdiv (ZERO, i)  = {quotient = ZERO, remainder = 0 }
    | sdiv (n, i) =
       let
          val (s, d) = sdivmod (n, i)
       in
          {quotient= s, remainder = d}
       end

   fun compare(TIMES10PLUS (n1, d1) , TIMES10PLUS  (n2, d2))=
       let
          val sub = compare(n1, n2)
       in
          if sub = EQUAL then
              if d1 = d2 then
                  EQUAL
              else
                  if d1 > d2 then
                      GREATER
                  else
                      LESS
          else
               sub
        end
     | compare(ZERO , TIMES10PLUS (n2, d2))= LESS
     | compare(TIMES10PLUS (n1, d1) , ZERO)= GREATER
     | compare(ZERO , ZERO)= EQUAL

   fun decimal(ZERO) = []
    | decimal(TIMES10PLUS (n,d))  =
      let
          val i = decimal n
      in
          i @ [d]
      end
   fun toString (ZERO) = "0"
    | toString (TIMES10PLUS (n, d)) =
       let
          val str = toString n
        in
          if str = "0" then
            Int.toString d
          else
            Int.toString d ^ str
        end
   fun invariant(ZERO) = true
    |  invariant(TIMES10PLUS (n, d))=  if d < 0 then
                                          false
                                       else
                                          invariant n

end
