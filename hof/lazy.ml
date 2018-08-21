(*
Lazy evaluation is useful in deferring computation unless needed. This has some advantages:
- Avoiding errors from ocurring
- Avoiding expensive computations which will not be used later on
- Creating potentially infinite data-structures
*)

(*
The following expression causes a Failure exception if the condition is true. Otherwise the value 1 is printed.
*)
if false then
  (print_int (1 / 0))
else
  (print_int 1)

(*
The following function eager evaluates the arguments before the function is called. Therefore both the following
calls to the functions cause Failure:
- eager true (1 / 0) 1
- eager false (1 / 0) 1
Note that the failure is independent of the condition. This is because OCaml follows 'eager evaluations' semantics
in case of function calls. This means that the arguments to a function are first computed before the function is
actually called.
*)

let eager_fun cond e1 e2 =
  if cond then
    print_int e1
  else print_int e2

(*
The following function lazy_fun is a higher order function that takes two functions th1 and th2 which wrap the expressions
to be evaluated under the two branches of the if. cond is the boolean condition that decides which branch is taken.
If cond is true, then th1 is called. If cond is false, then th2 is called.
Observe what happens when lazy_fun is called as the following:
- lazy_fun true (fun () -> 1/0) (fun () -> 1);;
- lazy_fun false (fun () -> 1/0) (fun () -> 1);;
Like in the top expression in this file, only the first call to lazy_fun fails because cond = true causes th1 to be called.
The second calls doesn't fail.
*)
let lazy_fun cond th1 th2 =
  if cond then
    print_int (th1 ())
  else print_int (th2 ())

(*
Parameters to functions implementing lazy evaluation strategy are of the form fun : unit -> 'a. Such functions are called
thunks.
*)
