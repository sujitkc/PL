exception Lexical_error

type token =
    NUM of float
  | ID  of string
  | IN

val string_of_token : token -> string

val lexer : (unit -> (char * char option)) -> token
