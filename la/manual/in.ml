(*
  The following scanner implements the scanner to scan 'in' keyword (as in OCaml).

  Please see num.ml for detailed notes on design of the scanner. id scanner has exactly the
  same design as the scanner for numbers in num.ml except the FSA it implements.
*)

let keywd_in () : State.state * ((char -> char option -> State.state) list) =
  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  and is_digit c = (c >= '0' && c <= '9') in
  let is_alphanum c = (is_alpha c) || (is_digit c) in
  let rec one (c : char) (lookahead : char option) : State.state =
    if c = 'i' then
      match lookahead with
        Some(c') ->
          if (c' = 'n') then     
            State.State(two)
          else
            State.Terminate(false)
      | None ->
          State.Terminate(false)
    else
      State.Terminate(false)

  and two (c : char) (lookahead : char option) : State.state =
    if (c = 'n') then
       match lookahead with
        Some(c') ->
          if (is_alphanum c') then     
            State.Terminate(false)
          else
            State.Terminate(true)
      | None -> State.Terminate(true)
    else
      State.Terminate(false)
  in
 (State.State(one), [two])
