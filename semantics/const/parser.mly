%{
%}

%token NEWLINE WS COMMA EOF LPAREN RPAREN COLON
%token ADD SUBTRACT
%token <int> INTEGER
%token <string> ID

%start expr
%type <Expression.expr> expr

%left ADD SUBTRACT

%% /* Grammar rules and actions follow */

expr :	  	
    expr ADD expr { Expression.Add($1, $3) } 
  | expr SUBTRACT expr { Expression.Subtract($1, $3) } 
  | INTEGER { Expression.Const $1 }
;

%%

