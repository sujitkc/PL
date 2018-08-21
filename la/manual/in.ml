(*
  The following scanner implements the scanner to scan 'in' keyword (as in OCaml).

  Please see num.ml for detailed notes on design of the scanner. id scanner has exactly the
  same design as the scanner for numbers in num.ml except the FSA it implements.
*)

let keywd_in (s : char Mystream.mystream) : State.state =
  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  and is_digit c = (c >= '0' && c <= '9') in
  let is_alphanum c = (is_alpha c) || (is_digit c) in
  let rec one (stream : char Mystream.mystream) : State.state =
    match stream with
      Mystream.End -> State.Terminate(false)
    | Mystream.Cons(c, _) ->
        let lookahead = (Mystream.tl stream) () in
        if c = 'i' then
          match lookahead with
            Mystream.Cons(c', _) ->
              if (c' = 'n') then     
                State.State(two)
              else
                State.Terminate(false)
          | Mystream.End ->
              State.Terminate(false)
        else
          State.Terminate(false)

  and two  (stream : char Mystream.mystream) : State.state =
    match stream with
      Mystream.End -> State.Terminate(true)
    | Mystream.Cons(c, _) ->
        let lookahead = (Mystream.tl stream) () in
        if (c = 'n') then
           match lookahead with
            Mystream.Cons(c', _) ->
              if (is_alphanum c') then     
                State.Terminate(false)
              else
                State.Terminate(true)
          | Mystream.End -> State.Terminate(true)
        else
          State.Terminate(false)
  in
  match s with
    Mystream.End -> State.Terminate(false)
  | _ -> one s

