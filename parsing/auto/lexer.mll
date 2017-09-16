{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*

rule scan = parse
  | integer as s {  Parser.NUM(int_of_string s) }
  | "+"  { Parser.ADD  }
  | "*"  { Parser.MUL }
  | eof  { Parser.EOF  }
  | _    { scan lexbuf }


{
}
