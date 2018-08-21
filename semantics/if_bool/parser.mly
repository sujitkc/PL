%{
%}

%token NEWLINE WS COMMA EOF LPAREN RPAREN COLON
%token IF THEN ELSE
%token ADD SUBTRACT
%token AND OR NOT
%token <int> INTEGER
%token <string> ID
%token <bool> BOOLEAN
%start expr
%type <Expression.expr> expr
%type <Expression.bool_expr> bool_expr
%left ADD SUBTRACT
%left AND OR
%right NOT

%% /* Grammar rules and actions follow */

expr :	  	
    expr ADD expr { Expression.Add($1, $3) } 
  | expr SUBTRACT expr { Expression.Subtract($1, $3) } 
  | INTEGER { Expression.Const $1 }
  | if_expr { $1 }
;

if_expr : IF bool_expr THEN expr ELSE expr { Expression.IfExpr($2, $4, $6) }

bool_expr:
    BOOLEAN { Expression.Boolean $1 }
  | bool_expr AND bool_expr { Expression.And($1, $3) }
  | bool_expr OR bool_expr { Expression.Or($1, $3) }
  | NOT bool_expr { Expression.Not($2) }
%%

