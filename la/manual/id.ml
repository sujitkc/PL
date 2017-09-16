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

let id (s : char Mystream.mystream) : State.state =
  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  and is_digit c = (c >= '0' && c <= '9') in
  let is_alphanum c = (is_alpha c) || (is_digit c) in
  let rec one (stream : char Mystream.mystream) : State.state =
    match stream with
      Mystream.End -> State.Terminate(false)
    | Mystream.Cons(c, _) ->
        let lookahead = (Mystream.tl stream) () in
        if is_alpha c then
          match lookahead with
            Mystream.Cons(c', _) ->
              if (is_alphanum c') then     
                State.State(two)
              else
                State.Terminate(true)
          | Mystream.End ->
              State.Terminate(true)
        else
          State.Terminate(false)

  and two  (stream : char Mystream.mystream) : State.state =
    match stream with
      Mystream.End -> State.Terminate(true)
    | Mystream.Cons(c, _) ->
        let lookahead = (Mystream.tl stream) () in
        if (is_alphanum c) then
           match lookahead with
            Mystream.Cons(c', _) ->
              if (is_alphanum c') then     
                State.State(two)
              else
                State.Terminate(true)
          | Mystream.End -> State.Terminate(true)
      else
        State.Terminate(true)
  in
  match s with
    Mystream.End -> State.Terminate(false)
  | _ -> one s

