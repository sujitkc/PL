(*
Emulating lazy semantics: fy and fz are thunks which help us delay the computation till the point where 
it is needed.
*)
let f x fy fz =
  if x then fy () else fz ()

(*
This is an alternative way to implement lazy semantics in OCaml. 
y and z are lazy expressions which can be forced to compute when needed.
*)
let g x y z =
  if x then (Lazy.force y) else (Lazy.force z)

(*
A stream is a lazy data-structure which can be useful to implement lists
which get created (extended) on demand. Streams are useful in implementing
long lists, or even infinite lists. Infinite lists are lists which can
potentially grow in size unboundedly. For example:
- A compiler reading an input program can't make any assumptions upfront
about its size.
- A browser receiving an HTML file or a JSON object from the server mayn't
 know how big it is going to be.
- A network router is configured to receive an unending stream of bits at
the input end.

Often receiving this data from input may be a time taking process. It may
not make sense to delay the further computation all the way till the time
when the reading of the inputs gets completed. Streams allow initiation
of other computation (probably in the same thread or a separate one)
even when the input is still being read in.
*)
type 'a stream =
    End
  | Cons of 'a * (unit -> 'a stream)

(*
The structure of the above variant type stream is similar to the linked
list that we had created. The first option takes care of the end of the list.
The second option is a pair: first part is a value corresponding to the
head of the lazy list. The second part is
a thunk which, when evaluated with give the stream representing the tail
of the lazy list. Encapsulating the tail in a thunk allows us to lazily 
compute the remainder of the list.
*)

let ns n k =
  let rec iter i =
    if i = k then End
    else Cons(n, fun () -> iter (i + 1))
  in
  iter 0
(*
Given below is a dump of session on the OCaml interpreter demonstrating how 
the above function can be used.

An important observation about this interaction with the stream is that everything
is purely functional here. All data created are immutable. All functions are pure,
which means that calling them any number of times with the same parameters will
return the same value.
*)
(*
# let s = ns "Sujit" 3;;
val s : string stream = Cons ("Sujit", <fun>)
# let f1 = match s with End -> failwith "done" | Cons(n, f) -> f;;
val f : unit -> string stream = <fun>
# let s1 = f1 ();;
val s1 : string stream = Cons ("Sujit", <fun>)
# let f2 = match s1 with End -> failwith "done" | Cons(n, f) -> f;;
val f2 : unit -> string stream = <fun>
# let s2 = f2 ();;
val s2 : string stream = Cons ("Sujit", <fun>)
# let f3 = match s2 with End -> failwith "done" | Cons(n, f) -> f;;
val f3 : unit -> string stream = <fun>
# let s3 = f3 ();;
val s3 : string stream = Cons ("Sujit", <fun>)
# let f4 = match s3 with End -> failwith "done" | Cons(n, f) -> f;;
val f4 : unit -> string stream = <fun>
# let s4 = f4 ();;
val s4 : string stream = End
# 
*)

(*
  Below is a sample function which demonstrates a simple scenario in which
  the stream ns is used. It just keeps extracting the values from the stream
  and printing that value until the end of the stream is reached.
  Try:
# get_all ns 10 20;; (* Should iterate through a lazy list of 10's, 20 elements in length *)
on the OCaml interpreter.
*)
let get_all st n1 n2 =
  let rec iter s =
    match s with
      End -> print_string "done"
    | Cons(n, f') -> print_string ((string_of_int n) ^ "\n"); iter (f' ())
  in
  iter (st n1 n2)

(*
  Finally, we implement xrange using the same stream data structure.
  get_all function can be used with xrange to see how it works.
  Try:
# get_all xrange 1 20;;
*)
let rec xrange n1 n2 =
  let rec iter i =
    if i = n2 then End
    else Cons(i, fun () -> iter (i + 1))
  in
  iter n1
