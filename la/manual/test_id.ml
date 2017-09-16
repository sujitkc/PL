let test_id s =
  let stream : (char Mystream.mystream) = Mystream.string_stream s in
  let rec iter f str =
    let result = f str in
      match result with
        State.Terminate(b) -> s, b
      | State.State(st)    -> iter st ((Mystream.tl str) ())
  in
  iter Id.id stream

let test_ids () =
  let n = [ "A"; "Aa"; "AaA"; "A1"; "Aa1"; "A1a"; "a1"; "1a"; "aa_1" ] in
  let result = List.map test_id n in
    List.iter (fun (x, y) -> (Printf.printf "%s -> %b;\n" x y)) result

let _ = test_ids ()
