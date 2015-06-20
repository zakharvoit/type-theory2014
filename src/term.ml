type term = Var of string
	  | Func of string * term list

let rec string_of_term = function
  | Var s       -> s
  | Func (f, e) -> f ^ "(" ^ Str.join ", " (List.map string_of_term e) ^ ")"

let rec print_equation p =
  print_string (string_of_term (fst p) ^ " = " ^ string_of_term (snd p)); print_newline ()

