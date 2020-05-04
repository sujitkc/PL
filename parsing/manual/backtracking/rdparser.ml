(*
  S ::= c A d
  A ::= a b
  A ::= a
*)

type result =
    Success of int
  | Failure

let string_of_result = function
    Success(_) -> "Success"
  | Failure -> "Failure"

let parse inp =
  let rec parse_S pos =
    Printf.printf "parse_S %d\n" pos;
    if (Bytes.get inp pos) = 'c' then
    begin
      match (parse_A1 (pos + 1)) with
        Success(pos') ->
          if (Bytes.get inp (pos' + 1)) = 'd' then
            Success(pos' + 1)
          else
            Failure
      | Failure ->
          match (parse_A2 (pos + 1)) with
            Success(pos') ->
              if (Bytes.get inp (pos' + 1)) = 'd' then
                Success (pos' + 1)
              else
                Failure
         | Failure -> Failure
    end
    else
      Failure
  and parse_A1 pos =
    Printf.printf "parse_A1 %d\n" pos;
    if Bytes.get inp pos = 'a' then
      if Bytes.get inp (pos + 1) = 'b' then
        Success(pos + 1)
      else
        Failure
    else
      Failure
  and parse_A2 pos =
    Printf.printf "<backtracking> parse_A2 %d\n" pos;
    if Bytes.get inp pos = 'a' then
      Success(pos)
    else
      Failure
  in
  try
    parse_S 0
  with
    Invalid_argument(_) -> Failure

let test inp =
   print_endline ("********** Parsing " ^ inp ^ " ***********");
   print_endline (inp ^ " --> " ^ (string_of_result (parse inp)));
   print_endline "*********************"


let main () =
  test "cabd";
  test "cad";
  test "cab";
  test "cac"

let _ = main ()   
