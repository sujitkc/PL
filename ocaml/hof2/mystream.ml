type 'a mystream =
    End
  | Cons of 'a * (unit -> 'a mystream)

let hd = function
    End -> failwith "empty"
  | Cons(h, _) -> h

let tl = function
    End -> failwith "empty"
  | Cons(_, t) -> t

let string_stream s =
  let rec iter n =
    if n >= String.length s then End
    else Cons (s.[n], fun () -> iter (n + 1))
  in
  iter 0
