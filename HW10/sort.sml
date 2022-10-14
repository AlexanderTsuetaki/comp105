functor PQSortFn(structure Q : PQUEUE) :> SORT where type elem = Q.elem =
struct
    type elem = Q.elem
    fun compare (s1,s2) = Q.compare_elem (s1, s2)
    fun sort (xs) = let
                      fun re ([]) = Q.empty
                        | re (x::xs) = Q.insert (x, re xs)
                      fun ls (q) = let
                                       val (x, newQueue) = Q.deletemin q
                                   in
                                      if Q.isEmpty q then
                                          []
                                       else
                                           x :: ls newQueue
                                  end
                  in
                      ls (re xs)
                  end

    (*simple minheapsort => put all values into heap,
                            then removemin until all values are in array*)
end
