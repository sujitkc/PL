%{
%}

%token ADD SUB MUL EOF
%token <int> NUM

%start expr
%type <Expr_type.expr_type> expr

%% /* Grammar rules and actions follow */
;
  expr  : term EOF        { $1          }

  term  : factor          { $1          }
        | factor ADD term { Expr_type.Add($1, $3) }

  factor: NUM             { Expr_type.Num($1)     }
        | NUM MUL factor  { Expr_type.Mul(Expr_type.Num($1), $3) } 
;

%%
