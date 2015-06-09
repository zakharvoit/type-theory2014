%{
module L = Lambda
%}

%token <string> TVar
%token TOpenPar TClosePar TLambda TDot TEof
%start only_expr
%type <Lambda.expr> only_expr
%%

only_expr: expr TEof { $1 }
      ;

expr: lambda         { $1 }
      | apply lambda { L.App ($1, $2) }
      | apply        { $1 }
      ;

lambda: TLambda TVar TDot expr { L.Lambda ($2, $4) }
      ;

apply: apply atom { L.App ($1, $2) }
     | atom       { $1 }
     ;

atom: TOpenPar expr TClosePar { $2 }
    | TVar                    { L.Var $1 }
    ;