let tss = [
  "1"        ;
  "1 + 2"    ;
  "1 + 2 + 3";
  "1 + 2 + " ; (* <error> *)
  "1 * 2"    ;
  "1 + 2 * 3";
  "1 * 2 + 3";
  "1 2"      ; (* <error> *)
  "1 * 2 + " ; (* <error> *)
  "1 + 2 * " ; (* <error> *)
]

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.expr Lexer.scan lexbuf) in
    Printf.printf "%s -> %d\n" s (Expr_type.val_of_expr result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s


let test_all () =
  List.iter test_parser tss

let _ = test_all ()
