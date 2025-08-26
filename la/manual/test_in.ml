let test_in s =
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
        let n = [ "in"; "int"; "in "; "in_"; "A1" ] in
  let result = List.map test_in n in
    List.iter (fun (x, y) -> (Printf.printf "%s -> %b;\n" x y)) result

let _ = test_ins ()
