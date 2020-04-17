(*
  The following scanner implements the scanner to scan identifiers. Identifiers in this language
  are strings have a leading alphabetic character, followed by zero or more alphanumeric 
  characters. For example:
  - Valid inputs: "A"; "Aa"; "AaA"; "A1"; "Aa1"; "A1a"; "a1"; 
  - Invalid inputs: "1a"; "a_b"

  Please see num.ml for detailed notes on design of the scanner. id scanner has exactly the
  same design as the scanner for numbers in num.ml except the FSA it implements.

  USAGE
  Please see test_id.ml to see how this scanner is used.
*)

let id () : State.state * ((char -> char option -> State.state) list) =
  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  and is_digit c = (c >= '0' && c <= '9') in
  let is_alphanum c = (is_alpha c) || (is_digit c) in
  let rec one (c : char) (lookahead : char option) : State.state =
    if is_alpha c then
      match lookahead with
        Some(c') ->
          if (is_alphanum c') then     
            State.State(two)
          else
            State.Terminate(true)
      | None ->
          State.Terminate(true)
    else
      State.Terminate(false)
  and two  (c : char) (lookahead : char option) : State.state =
    if (is_alphanum c) then
      match lookahead with
        Some(c') ->
           if (is_alphanum c') then     
             State.State(two)
           else
             State.Terminate(true)
          | None -> State.Terminate(true)
    else
      State.Terminate(true)
  in
  (State.State(one), [two])
