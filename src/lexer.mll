{
  open Parser
}

let whitespace = [' ' '\t' '\r']+
let function   = ['a'-'h']['a'-'z' '0'-'9']*
let variable   = ['i'-'z']['a'-'z' '0'-'9']*

rule token = parse
             | whitespace       { token lexbuf }
             | variable as var  { TVar var }
             | function as func { TFunc func }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
	     | ','              { TComma }
             | '='              { TEqual }
             | eof              { raise End_of_file }
	     | '\n'             { TEoln }
