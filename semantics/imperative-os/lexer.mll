{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '('          { (* print_endline "("; *)       Parser.LPAREN                     }
  | ')'          { (* print_endline ")";*)        Parser.RPAREN                     }
  | '{'          { (* print_endline "{";*)        Parser.LBRACE                     }
  | '}'          { (* print_endline "}";*)        Parser.RBRACE                     }
  | ":="         { (* print_endline ":=";*)       Parser.ASSIGN                     }
  | ':'          { (* print_endline ":";*)        Parser.COLON                      }
  | ';'          { (* print_endline ";";*)        Parser.SEMICOLON                  }
  | '-'          { (* print_endline "-";*)        Parser.SUBTRACT                   }
  | '+'          { (* print_endline "+";*)        Parser.ADD                        }
  | '='          { (* print_endline "=";*)        Parser.EQ                         }
  | '<'          { (* print_endline "<";*)        Parser.LT                         }
  | integer as s { (* print_endline s;*)          Parser.INTEGER((int_of_string s)) }
  | "if"         { (* print_endline "if";*)       Parser.IF                         }
  | "then"       { (* print_endline "then";*)     Parser.THEN                       }
  | "else"       { (* print_endline "else";*)     Parser.ELSE                       }
  | "true"       { (* print_endline "true";*)     Parser.BOOLEAN(true)              }
  | "false"      { (* print_endline "false";*)    Parser.BOOLEAN(false)             }
  | "and"        { (* print_endline "and";*)      Parser.AND                        }
  | "or"         { (* print_endline "or";*)       Parser.OR                         }
  | "not"        { (* print_endline "not";*)      Parser.NOT                        }
  | "let"        { (* print_endline "let";*)      Parser.LET                        }
  | "boolean"    { (* print_endline "boolean";*)  Parser.TYPE(Expression.Boolean)   }
  | "int"        { (* print_endline "int";*)      Parser.TYPE(Expression.Integer)   }
  | "in"         { (* print_endline "in";*)       Parser.IN                         }
  | "return"     { (* print_endline "return";*)   Parser.RETURN                     }
  | "while"      { (* print_endline "while"; *)   Parser.WHILE                      }
  | id as s      { (* print_endline s;*)          Parser.ID(s)                      }
  | ','          { (* print_endline ",";*)        Parser.COMMA                      }
  | eof          { (* print_endline "EOF"; *)     Parser.EOF                        }

{
}
