let test_lexer s =
  let buffer = Mybuffer.from_string s in
  try
    let tok = Lexer.lexer buffer in
    Printf.printf "%s -> true;\n" (Lexer.string_of_token tok)
  with
    Lexer.Lexical_error -> print_string (s ^ " -> false;\n")

let test_lexers () =
  let inputs = [ "A"; "Bb"; "BbB"; "C1"; "Dd2"; "E3e"; "f4"; "5g"; "G_"; "6";
                 "7.7"; "88.8"; "99.99"; ".11"; "."; "1."; "in"; "integral" ] in
  List.iter test_lexer inputs; 
  print_string (string_of_int (List.length inputs) ^ " test cases.\n")

let _ = test_lexers ()
