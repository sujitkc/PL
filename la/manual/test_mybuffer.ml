#use "mybuffer.ml"

let test_from_string () =
  print_endline "test_from_string";
  let buffer = from_string "Bangalore" in
  let rec loop () =
      let c, la = buffer () in
      let str_la =
      match la with
        None -> "nil"
      | Some(c') -> Bytes.make 1 c' in
      begin
        Printf.printf "Current character = %c, lookahead = %s\n" c str_la;
        loop ()
      end
  in
  try
    loop ()
  with
    End_of_buffer -> print_endline "end of buffer"

let test_from_file () =
  print_endline "test_from_file";
  let buffer = from_file "mybuffer.mli" in
  let rec loop () =
      let c, la = buffer () in
      let str_la =
      match la with
        None -> "nil"
      | Some(c') -> Bytes.make 1 c' in
      begin
        Printf.printf "Current character = %c, lookahead = %s\n" c str_la;
        loop ()
      end
  in
  try
    loop ()
  with
    End_of_buffer -> print_endline "end of buffer"


let main () =
  test_from_string ();
  test_from_file ()

let _ = main ()
