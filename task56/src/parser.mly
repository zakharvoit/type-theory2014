%{
module T = Term
%}

%token <string> TVar TFunc
%token TOpenPar TClosePar TComma TEof TEqual
%start equation
%type <Term.term * Term.term> equation
%%

equation: term TEqual term { ($1, $3) };
term: TVar { T.Var $1 }
    | TFunc TOpenPar terms TClosePar { T.Func ($1, List.rev $3) }
    ;
terms: term { [$1] }
     | terms TComma term { $3 :: $1 }
