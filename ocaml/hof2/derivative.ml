
(* 1. Implement a function derivative approximating the mathematical differentiation
D(f(x)) = (f(x + delta) - f(x)) / delta where delta is a small number, e.g. 0.0001

2. Implement a double differential function.
*)


let d f x =
  let dx = 0.00001
  in
  ((f (x +. dx)) -. (f x)) /. dx

(*
let dd f x =
  ((d f (x + dx)) - ((d f x) /. dx) /. dx)
*)

let derivative f =
  let dx = 0.00001
  in
  fun x -> ((f (x +. dx)) -. (f x)) /. dx

let dd f = derivative (derivative f)

let td f = derivative (dd f)
