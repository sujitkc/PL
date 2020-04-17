let test_id s =
  let buffer = Mybuffer.from_string s in
  let (init, accept_states) = Id.id () in
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

let test_ids () =
  let ss = [ "A"; "Aa"; "AaA"; "A1"; "Aa1"; "A1a"; "a1"; "1a"; "aa_1" ] in
  let result = List.map test_id ss in
    List.iter (fun (x, y) -> (Printf.printf "%s -> %b;\n" x y)) result

let _ = test_ids ()
