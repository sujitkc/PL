(*
Book for OCaml:
- Real World OCaml
- Developing Applications with Objective Caml
- Introduction to Objective Caml -- J. Hickey
*)

(* Higher order functions activity
 1. Use List.map to compute a new list with each element a square of the original list.
 2. Use List.filter to compute a sub-list of odd elements of the original list.
*)

let rec range n1 n2 =
  if n1 = n2 then []
  else n1 :: (range (n1 + 1) n2)

let range_tr n1 n2 =
  let rec iter n1 n2 acc =
    if n1 = n2 then acc
    else iter (n1 + 1) n2 (n1 :: acc)
  in
  iter n1 n2 []

let rec is_there e l =           
    match l with
      [] -> false             
    | h :: t -> if e = h then
                  true
                else
                  (is_there e t)

let rec length l =
  match l with
    [] -> 0
  | _ :: t -> 1 + (length t)

let rec length1 = function
    [] -> 0
  | _ :: t -> 1 + (length1 t)

let append l1 l2 =
 (* helper function *)
 let rec aux l1 = function
    [] -> l1
  | h :: t -> aux (h::l1) t
 in
 match l2 with
    [] -> l1
  | _  -> List.rev (aux (List.rev l1) l2)

(* let _ = (Printf.printf "%b\n" (is_there 3 [1;2;3]));; *)
