
(*****A*****)
fun mynull []= true
    | mynull (xs)= false;

(*****B*****)
fun reverse  xs = foldl op::nil xs;

fun minlist [] = raise Match
    | minlist(x::xs) =  foldl Int.min x xs;


(*****C*****)
exception Mismatch
fun zip ([],[])  = []
    | zip (x::xs, y::ys) = (x,y)::zip(xs,ys)
    | zip (x,[]) = raise Mismatch
    | zip ([],x) = raise Mismatch;

(*****F*****)
datatype sx = SYMBOL of string
         | NUMBER of int
         | BOOL   of bool
         | SXLIST of sx list;
(** if empty return an empty sxlist **)
 fun numbersSx [] = SXLIST []
(** if not-empty append a the sx to another sx **)
     |numbersSx (x::xs) = SXLIST (map (fn x => NUMBER x) xs);

fun flattenSyms (SXLIST[]) = []
    |flattenSyms (SXLIST(x::xs)) = flattenSyms(x)@flattenSyms(SXLIST xs)
    |flattenSyms (SYMBOL x) = [x]
    |flattenSyms (NUMBER x) =[]
    |flattenSyms (BOOL x) = [];


fun sxString (SYMBOL s) = s
    | sxString (NUMBER n) = Int.toString n
    | sxString (BOOL b) = if b then "true" else "false"
    | sxString (SXLIST sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")";

(*****H*****)
datatype nat = ZERO
             | TIMES10PLUS of nat * int;

fun times10 n = TIMES10PLUS (n, 0);
fun times10plus (ZERO,0)= ZERO
    | times10plus(m,d) = TIMES10PLUS(m,d);
fun natOfDigit d = TIMES10PLUS (ZERO, d);
fun flip f (x,y) = f(y, x)
fun natOfDigits ds = foldl ( flip TIMES10PLUS) ZERO ds

(**1**)

fun intOfNat (ZERO) = 0
    | intOfNat (TIMES10PLUS(n,i)) = i + intOfNat(n) * 10;

(**2**)
val it = natOfDigits [1,2,3,4];
val it2 = intOfNat(natOfDigits [1,2,3,4]);

(**3**)
fun natOfInt 0 = ZERO
    | natOfInt n =  TIMES10PLUS(natOfInt(n div 10), n mod 10);

(**4**)
val it3 = intOfNat(natOfInt 123);

(**5**)
fun natString ZERO = ""
    |natString (TIMES10PLUS(n,i)) = natString(n) ^ Int.toString (i);

(**6**)
val it4 = natString(natOfInt 1234);
(*****I*****)
fun carryIntoNat (TIMES10PLUS(n,d), 0 ) = n
    | carryIntoNat (ZERO, c) = natOfInt(c)
    | carryIntoNat (TIMES10PLUS(n,d), c) = TIMES10PLUS(
                                            carryIntoNat(n,(d+1) div 10)
                                                          ,(d+1) mod 10);

fun addWithCarry (ZERO, ZERO, c) = ZERO
    |addWithCarry (TIMES10PLUS(n,d), ZERO, c) = carryIntoNat(TIMES10PLUS(n,d),c)
    | addWithCarry (ZERO, TIMES10PLUS(m,e), c) = carryIntoNat(TIMES10PLUS(m,e),c)
    | addWithCarry (TIMES10PLUS(n,d), TIMES10PLUS(m,e), c) =
              let val x1 = (d + e + c) mod 10
                  val x2 = (d + e + c) div 10
              in TIMES10PLUS(addWithCarry(n, m, x2),x1)
              end;

val it5  = addWithCarry(natOfInt(123),natOfInt(234), 0);
val it6  = intOfNat(addWithCarry(natOfInt(123),natOfInt(234), 0));

fun addNats (TIMES10PLUS(n,d), TIMES10PLUS(m,e))
            = addWithCarry(TIMES10PLUS(n,d),TIMES10PLUS(m,e),0)
   |addNats (ZERO,ZERO) = ZERO
   |addNats (ZERO,TIMES10PLUS(n,d)) = carryIntoNat(TIMES10PLUS(n,d),0)(*todo*)
   |addNats (TIMES10PLUS(n,d),ZERO) = carryIntoNat(TIMES10PLUS(n,d),0)
val it6  = intOfNat(addNats(natOfInt(123),natOfInt(234)));

exception Negative
fun borrowFromNat (TIMES10PLUS(n,d), 0) = TIMES10PLUS(n,d)
    |borrowFromNat (TIMES10PLUS(n,0), 1) = TIMES10PLUS(borrowFromNat(n,1), 9)
    |borrowFromNat (TIMES10PLUS(n,d), 1) = TIMES10PLUS(n,d-1)
    |borrowFromNat (_, _) = raise Negative
(**val it7  = intOfNat(borrowFromNat(ZERO,1));**)

fun subWithBorrow (TIMES10PLUS(n,d), ZERO, b)
                                = borrowFromNat(TIMES10PLUS(n,d),b)
    |subWithBorrow (ZERO,_,b) = raise Negative
    |subWithBorrow (TIMES10PLUS(n,d), TIMES10PLUS(m,e), b)
                                = let val x1 = (d - e - b) mod 10
                                      val x2 = if (d - e - b) < 0 then 1 else 0
                                  in TIMES10PLUS(subWithBorrow(n, m, x2),x1)
                                  end;

fun subNats (n1,n2) = subWithBorrow(n1,n2,0);




fun opsAgree name intop natop n1 n2 =
    Unit.checkExpectWith Int.toString name
    (fn () => intOfNat (natop (natOfInt n1, natOfInt n2)))
    (intop (n1, n2) handle Overflow => 0);

val () = opsAgree "123 + 2018" (op +) addNats 123 2018;



val () = Unit.reportWhenFailures ();
