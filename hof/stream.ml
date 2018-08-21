(*
  A stream is a data structure that provides the elements of a 
  potentially infinite linear data (e.g. list) on demand.

  In functional programming, streams can be created using lazy 
  evaluation. The stream is just a pair of a the next piece of data
  in the list, and a thunk encapsulating the remaining stream. So, 
  it's a recursive data-structure as can be seen below.
*)

type 'a mystream =
    Empty
  | Cons of 'a * (unit -> 'a mystream)


let hd = function
    Empty -> failwith "empty"
  | Cons(h, _) -> h

let tl = function
    Empty -> failwith "empty"
  | Cons(_, t) -> t

let rec ns n = Cons(n, fun () -> ns n)



(*
A stream of 2's
---------------






# let s = ns 2;;    
val s : int mystream = Cons (2, <fun>)
# let s = tl s ();;
val s : int mystream = Cons (2, <fun>)
# hd s;;
- : int = 2
# let s = tl s ();;
val s : int mystream = Cons (2, <fun>)
# hd s;;           
- : int = 2

A stream of evens and odds
--------------------------
# let rec startwith n = Cons(n, fun () -> startwith (n + 2));;
val startwith : int -> int mystream = <fun>
  Evens
  -----
# let evens = startwith 0;;                                   
val evens : int mystream = Cons (0, <fun>)
# hd evens;;        
- : int = 0
# let e = tl evens ();;
val e : int mystream = Cons (2, <fun>)
# hd e;;
- : int = 2
# let e = tl e ();;    
val e : int mystream = Cons (4, <fun>)
# let e = tl e ();;
val e : int mystream = Cons (6, <fun>)
# let e = tl e ();;
val e : int mystream = Cons (8, <fun>)

  Odds
  ----
# let odds = startwith 1;;
val odds : int mystream = Cons (1, <fun>)
# hd odds;;   
- : int = 1
# let o = tl odds ();;    
val o : int mystream = Cons (3, <fun>)
# let o = tl o ();;   
val o : int mystream = Cons (5, <fun>)
# let o = tl o ();;
val o : int mystream = Cons (7, <fun>)


(* iterator *)
# let rec iter = function                   
    [] -> failwith "empty"                  
   | h :: t -> Cons (h,(fun () -> iter t));;
val iter : 'a list -> 'a mystream = <fun>
# let i = iter ([1;2;3]);;
val i : int mystream = Cons (1, <fun>)
# let i = tl i;;   
val i : unit -> int mystream = <fun>
# let i = i ();;
val i : int mystream = Cons (2, <fun>)
# let i = (tl i) ();;
val i : int mystream = Cons (3, <fun>)
# let i = (tl i) ();;
Exception: Failure "empty".
# 
*)
