type token =
    NUM of float
  | ID  of string
  | IN

val string_of_token : token -> string

val lexer : char Mystream.mystream -> token * (char Mystream.mystream)
