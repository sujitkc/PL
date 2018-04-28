%{
%}

%token          NEWLINE WS COMMA EOF LPAREN RPAREN LBRACE RBRACE SEMICOLON ASSIGN COLON
%token          IF THEN ELSE LET IN RETURN
%token          ADD SUBTRACT EQ
%token          AND OR NOT
%token <int>    INTEGER
%token <string> ID
%token <bool>   BOOLEAN
%token <Expression.typ>    TYPE

%type <Expression.Program.program> prog
%type <Expression.expr> expr

%start prog

%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */

prog : decls fundefs {
   {
      Expression.Program.decls    = $1;
      Expression.Program.fundefs = $2;
    }
  }
  ;

decls :
  nonemptydecls                    { List.rev $1 }
  |                                  { []          }
  ;

nonemptydecls :
    nonemptydecls decl             { $2 :: $1  }
  | decl                             { [ $1 ]    }
  ;
  
decl : TYPE ID SEMICOLON                   { ($2, $1) }
  ;

fundefs :
  nonemptyfundefs                    { List.rev $1 }
  |                                  { []          }
  ;

nonemptyfundefs :
    nonemptyfundefs fundef             { $2 :: $1  }
  | fundef                             { [ $1 ]    }
  ;
  
fundef : TYPE ID LPAREN paramlist RPAREN stmt  {
    {
      Expression.Program.fname  = $2;
      Expression.Program.rtype  = $1;
      Expression.Program.params = $4;
      Expression.Program.body   = $6;
    }
  }
  ;

paramlist :
    nonemptyparamlist                { List.rev $1                  }
  |                                  { []                           }
  ;

nonemptyparamlist :
    nonemptyparamlist COMMA param    { $3 :: $1                     }
  | param                            { [ $1 ]                       }
  ;

param :
    TYPE ID                          { ($2, $1)                     }
  ;
  
stmts :
    nonemptystmts                    { List.rev $1                  }
  |                                  { []                           }
  ;

nonemptystmts :
    nonemptystmts stmt               { $2 :: $1                     }
  | stmt                             { [$1]                         }
  ;

stmt :
    ID ASSIGN expr SEMICOLON         { Expression.Program.Assignment($1, $3) }
  | RETURN expr SEMICOLON            { Expression.Program.Return($2)         }
  | LBRACE stmts RBRACE              { Expression.Program.Block($2)          }
  ;

expr :	  	
  | ID                               { Expression.Id($1)             } 
  | INTEGER                          { Expression.IntConst($1)       }
  | BOOLEAN                          { Expression.BoolConst $1       }
  | expr ADD expr                    { Expression.Add($1, $3)        }
  | expr SUBTRACT expr               { Expression.Sub($1, $3)        }
  | IF expr THEN expr ELSE expr      { Expression.If($2, $4, $6)     }
  | expr AND expr                    { Expression.And($1, $3)        }
  | expr OR expr                     { Expression.Or($1, $3)         }
  | NOT expr                         { Expression.Not($2)            }
  | expr EQ expr                     { Expression.Equals($1, $3)     }
  | LET ID EQ expr IN expr           { Expression.Let($2, $4, $6)    }
  | LPAREN expr RPAREN               { $2                            }
  | ID LPAREN arglist RPAREN         { Expression.FunCall($1, $3)    }
;

arglist :
    nonemptyarglist                  { List.rev $1                   }
  |                                  { []                            }
  ;

nonemptyarglist :
    nonemptyarglist COMMA arg        { $3 :: $1                      }
  | arg                              { [$1]                          }
  ;

arg : expr                           { $1                            }
  ;
%%
