(* To illustrate dynamic programming through Fibonacci number *)

(*
The purely functional implementation of the fibonacci number function has exponential
time complexity. The methods we have discussed so far aren't helpful in improving its
performance.*)
let rec fibonacci1 n =
  match n with
    0 | 1 -> 1
  |     _ -> (fibonacci1 (n - 1)) + (fibonacci1 (n - 2))

(* 
Dynamic programming is a well-known technique that achieves speed-up
by storing results computed earlier.

The memoise function shown below returns a returns a function which maintains
a mutable list of key-value pairs (key standing for the input to the computation
and value for the corresponding output). This function looks up in the table
if it has already stored the result corresponding to the parameter passed. 
If it finds it, it is returned, else the result is computed, stored in the
table and the result is returned.
*)
let memoise f =
  let table = ref []
  in
  let rec find tab n =
    match tab with
    | []           -> 
            let v = (f n)
            in
            table := (n, v) :: !table;
            v
    | (n', v) :: t -> 
            if n' = n then v else (find t n)
  in
  fun n -> find !table n
     
(* Fibonacci with higher order generic memoise function *)
let fibonacci2 = memoise fibonacci1



(*
Another functionally equivalent version of the memoise function which uses the standard library module
Hashtbl.
The difference between memoise and memoize is that the average access time to
Hashtbl is O(1) as opposed to an average access time of O(n) in memoise.
*)
let memoize f =
    let table = Hashtbl.create 30 in
    (fun x ->
      try
        Hashtbl.find table x with
      | Not_found ->
        let y = f x in
        Hashtbl.add table x y;
        y
    )

(*
The problem with the above way of using dynamic programming is that once a value is not
found in it, it makes a call to fibonacci1, which internally makes recursive calls to 
itself leading to bad performance. For example:

*)
(* fibonacci with non-generic memoisation *)
let fibonacci3 n = 
  let table = ref [] in
  let rec fib3 n =
   let rec f3 n' =
      match n' with
        0 | 1 -> 1
      | _     -> (fib3 (n' - 1)) + (fib3 (n' - 2))
    in
    let rec find tab n'' =
      match tab with
      | []           -> 
              let v = (f3 n'')
              in
              table := (n'', v) :: !table;
              v
      | (n''', v) :: t -> 
              if n''' = n'' then v else (find t n'')
    in
    find !table n
  in
  fib3 n

(*
A time function to compute the time taken by a computation. Uses lazy evaluation using
a thunk. Use it as follows:
# time (fun () -> fibonacci3 400);;
result = 4536716983099355453 time taken = 0.004- : unit = ()
*)
let time f =
    let s = Sys.time () in
    let result = (f ()) in
    let e = Sys.time () in
    print_string ("result = " ^ (string_of_int result) ^ " time taken = " ^ (string_of_float (e -. s)))

