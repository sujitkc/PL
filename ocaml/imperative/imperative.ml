(*
Implement a function make_counter which takes an integer n and returns a counter which counts upto n.
*)

(*
The below functions returns a function (actually, a closure) which makes use of a reference cell 'count'.
The function returned will update the value in this reference cell, and return the updated value whenever
called. On count value reaching n, the counter raises a Failure exception (using a failwith call).
*)
let make_counter n =
  let count = ref 0 in
  let counter () =
    if !count = n then failwith "done!"
    else
      count := !count + 1;
      !count
  in
  counter

(*
Here's a session on the OCaml interpreter demonstrating interaction with the make_counter function
# #use "imperative.ml";;
val make_counter : int -> unit -> int = <fun>
val use_counter : unit -> unit = <fun>
# let c3 = make_counter 3;;
val c3 : unit -> int = <fun>
# c3 ();;
- : int = 1
Observations:
1) The above may mistakenly be likened with static variables in C/C++ -- variables with local scope
and global lifetimes. To appreciate the difference, consider the continuation of the above session
on the OCaml interpreter:

# let c4 = make_counter 4;;
val c4 : unit -> int = <fun>
# let c5 = make_counter 5;;
val c5 : unit -> int = <fun>
# c4 ();;
- : int = 1
# c5 ();;
- : int = 1
# c4 ();;
- : int = 1
#c5 ();;
- : int = 2

Note that both the counters c4 and c5 can co-exist with different values of their counter states.
With static variables in C, this would not be possible.

2) To understand why the support for closures is more powerful in OCaml than in Python, try implementing
the above function in Python. What do you observe?
*)

(*
The following function demonstrates a canonical client function for the make_counter function.
*)
let use_counter () =
    let c3 = make_counter 3 in
    let rec iter () =
      try            
        let count = c3 () in          
        (
          print_string ("count = " ^ (string_of_int count) ^ "\n");
          iter ()
        )
      with
        Failure(msg) -> print_string msg
    in
    iter ()

