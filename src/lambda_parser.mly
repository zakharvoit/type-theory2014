%{
module L = Lambda
%}

%token <string> TVar
%token TOpenPar TClosePar TLambda TDot TEof
%token TOpenBracket TCloseBracket TAssign
%start only_expr
%start expr_with_assign
%type <Lambda.expr> only_expr
%type <Lambda.expr * string * Lambda.expr> expr_with_assign
%%

expr_with_assign: expr TOpenBracket TVar TAssign expr TCloseBracket
                      { ($1, $3, $5) }
                ;

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