{
exception Lexer_exception of string

type token =
    NUM of int
  | ADD
  | MUL
  | LPAREN
  | RPAREN
  | EOF
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*

rule scan = parse
  | integer as s {  NUM(int_of_string s) }
  | "+"  { ADD  }
  | "*"  { MUL }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | eof  { EOF  }
  | _    { scan lexbuf }


{
}
