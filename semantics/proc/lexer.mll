{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf                  }
  | '('          { Parser.LPAREN                     }
  | ')'          { Parser.RPAREN                     }
  | ':'          { Parser.COLON                      }
  | '-'          { Parser.SUBTRACT                   }
  | '+'          { Parser.ADD                        }
  | '='          { Parser.EQ                         }
  | "->"         { Parser.RTARROW                    }
  | integer as s { Parser.INTEGER((int_of_string s)) }
  | "if"         { Parser.IF                         }
  | "then"       { Parser.THEN                       }
  | "else"       { Parser.ELSE                       }
  | "true"       { Parser.BOOLEAN(true)              }
  | "false"      { Parser.BOOLEAN(false)             }
  | "and"        { Parser.AND                        }
  | "or"         { Parser.OR                         }
  | "not"        { Parser.NOT                        }
  | "let"        { Parser.LET                        }
  | "in"         { Parser.IN                         }
  | "fun"        { Parser.FUN                        }
  | id as s      { Parser.ID(s)                      }
  | ','          { Parser.COMMA                      }
  | eof          { Parser.EOF                        }

{
}
