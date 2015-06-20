type term = Var of string
	  | Func of string * term list

let rec string_of_term = function
  | Var s       -> s
  | Func (f, e) -> f ^ "(" ^ Str.join ", " (List.map string_of_term e) ^ ")"
