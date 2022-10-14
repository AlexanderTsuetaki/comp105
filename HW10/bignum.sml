functor BignumFn(structure N : NATURAL) :> BIGNUM
  =
struct
  type bigint = bool * N.nat
  exception BadDivision
  exception myFailure
  fun ofInt (n) = if n < 0 then
                    (false, N.ofInt( n * (0 - 1) ))
                  else
                    (true, N.ofInt n)

  fun negated (s, n) = (not s, n)

  fun <+> (n1, n2) = raise myFailure
  fun <-> (n1, n2)= raise myFailure
  fun <*> (n1, n2)= raise myFailure
  fun sdiv (n1, i)= raise myFailure
  fun toString (n1) = raise myFailure
  fun compare ((true, m1), (true, m2)) = N.compare (m1, m2)
   |  compare ((false, m1), (true, m2)) = LESS
   |  compare ((true, m1), (false, m2)) = GREATER
   |  compare ((false, m1), (false, m2)) = N.compare (m2, m1)

  (*fun <+> ((true, m1) , (true, m2))   = (true , m1 N./+/ m2)
    | <+> ((true, m1) , (false, m2))  = if N.compare (m2,m1) = LESS
                                        then
                                          (false , m1 N./-/ m2)
                                       else
                                          (true, m2 N./-/ m1)
    | <+> ((false, m1), (true, m2))  = if m1 > m2 then
                                          (false, m1 N./-/ m2)
                                       else
                                          (true, m2 N./-/ m1)
    | <+> ((false, m1), (false, m2)) = (false, m1 N./+/ m2)

  fun <-> ((true, m1), (true, m2)) =  if m1 >= m2 then
                                          (true, m1 N./-/ m2)
                                       else
                                          (false, m2 N./-/ m1)
   |  <-> ((true, m1), (false, m2)) = (true, m1 N./+/ m2)
   |  <-> ((false, m1), (true, m2)) = (false, m1 N./+/ m2)
   |  <-> ((false, m1), (false, m2)) = if m1 >= m2 then
                                          (true, m1 N./-/ m2)
                                       else
                                          (false, m2 N./-/ m1)


  fun <*> ((true, m1), (true, m2))   = (true, m1 N./*/ m2)
   |  <*> ((true, m1), (false, m2))  = (false, m1 N./*/ m2)
   |  <*> ((false, m1), (true, m2))  = (false, m1 N./*/ m2)
   |  <*> ((false, m1), (false, m2)) = (true, m1 N./*/ m2)


 fun sdiv ((true, m1), i)   = (true, N.sdiv (m1, i))
  | sdiv ((true, m1), i)  = (false, N.sdiv (m1, i))
  | sdiv ((false, m1), i)  = (false, N.sdiv (m1, i))
  | sdiv ((false, m1), i) = (true, N.sdiv (m1, i))



  fun toString (s, n) = if s then
                           N.toString n
                        else
                          "-" ^ N.toString n
*)

  fun invariant (s, n) = if not s andalso (N.compare (n, N.ofInt 0) = EQUAL)then
                              false
                         else
                              N.invariant n

  infix 6 <x> <->
  infix 7 <*> sdiv

  val /+/ = N./+/
  val /-/ = N./-/
  val /*/ = N./*/

  infix 6 /+/ /-/
  infix 7 /*/

end
