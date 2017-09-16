let test_string_stream () =
  let s = Mystream.string_stream "Sujit" in
  let rec iter stream =
    try
      Printf.printf "%c\n" (Mystream.hd stream);
      iter ((Mystream.tl stream) ())
    with Failure(_) -> ()
  in
  iter s

let _ = test_string_stream ()
