(*
  Function to run one test case. It converts the input string to a char stream,
  and calls Num.num repeatedly (iteration implemented using a tail-recursive 
  function iter). On every iteration it passes the stream advanced by one char
  w.r.t. to the previous.
*)
let test_num s =
  let stream = Mystream.string_stream s in
  let rec iter f str =
    let result = f str in
      match result with
        State.Terminate(b) -> s, b
      | State.State(st) -> iter st ((Mystream.tl str) ())
  in iter Num.num stream

(*
  Function to run a test suite containing an arbitrary list of test inputs. Each test 
  case in the test suite should be carefully chosen, so that running all the test cases
  in the test suite provides a comprehensive coverage of the functionality of the 
  module. The systematic methods for selecting appropriate test inputs is part of the
  study in Software Testing, and is beyond the scope of this discussion.
*)
let test_nums () =
  let n = [ "1"; "1.1"; "11.1"; "11.11"; ".11"; "."; "1."; "B" ] in
  let result = List.map test_num n in
    List.iter (fun (x, y) -> (Printf.printf "%s -> %b;\n" x y)) result

let _ = test_nums ()
