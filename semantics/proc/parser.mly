%{
%}

%token          NEWLINE WS COMMA EOF LPAREN RPAREN COLON
%token          IF THEN ELSE LET IN FUN RTARROW
%token          ADD SUBTRACT EQ
%token          AND OR NOT
%token <int>    INTEGER
%token <string> ID
%token <bool>   BOOLEAN
%start expr
%type <Expression.expr> expr
%type <Expression.expr> bool_expr

%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */

expr :	  	
  | ID                               { Expression.Id($1)          } 
  | INTEGER                          { Expression.IntConst $1     }
  | expr ADD expr                    { Expression.Add($1, $3)     }
  | expr SUBTRACT expr               { Expression.Sub($1, $3)     }
  | LPAREN expr RPAREN               { $2;                        }
  | IF bool_expr THEN expr ELSE expr { Expression.If($2, $4, $6)  }
  | LET ID EQ expr IN expr           { Expression.Let($2, $4, $6) }
  | FUN ID RTARROW expr              { Expression.FunDef($2, $4)  }
  | expr expr                        { Expression.FunApp($1, $2)  }
;

bool_expr:
    BOOLEAN { Expression.BoolConst $1 }
  | bool_expr AND bool_expr { Expression.And($1, $3) }
  | bool_expr OR bool_expr { Expression.Or($1, $3) }
  | NOT bool_expr { Expression.Not($2) }
;
%%
