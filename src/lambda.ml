type expr = Lambda of string * expr
          | App of expr * expr
          | Var of string

let rec string_of_expr = function
  | Var s         -> s
  | App (a, b)    -> "(" ^ string_of_expr a ^ " " ^ string_of_expr b ^ ")"
  | Lambda (s, a) -> "(\\" ^ s ^ "." ^ string_of_expr a ^ ")"
