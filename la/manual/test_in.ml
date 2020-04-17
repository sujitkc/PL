let test_in s =
(*
  let stream : (char Mystream.mystream) = Mystream.string_stream s in
  let rec iter f str =
    let result = f str in
      match result with
        State.Terminate(b) -> s, b
      | State.State(st)    -> iter st ((Mystream.tl str) ())
  in
  iter In.keywd_in stream
*)
  let buffer = Mybuffer.from_string s in
  let (init, accept_states) = In.keywd_in () in
  let rec loop sS = 
    match sS with
      State.Terminate(tf) -> tf
    | State.State(s') ->
        try
          let c, la = buffer () in
          loop (s' c la)
        with
          Mybuffer.End_of_buffer -> List.mem s' accept_states
  in
  s, (loop init)


let test_ins () =
  let n = [ "in"; "int"; "in "; "A1" ] in
  let result = List.map test_in n in
    List.iter (fun (x, y) -> (Printf.printf "%s -> %b;\n" x y)) result

let _ = test_ins ()
