{
  open Parser
}

let whitespace = [' ' '\t' '\r' '\n']
let variable   = ['a'-'z']['a'-'z' '0'-'9']*

rule token = parse
             | whitespace       { token lexbuf }
             | variable as var  { TVar var }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
             | '.'              { TDot }
             | '\\'             { TLambda }
             | ":="             { TAssign }
             | '['              { TOpenBracket }
             | ']'              { TCloseBracket }
             | eof              { TEof }
