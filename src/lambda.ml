module H = Hashtbl

type expr = Lambda of string * expr
          | App of expr * expr
          | Var of string

let rec string_of_expr = function
  | Var s         -> s
  | App (a, b)    -> "(" ^ string_of_expr a ^ " " ^ string_of_expr b ^ ")"
  | Lambda (s, a) -> "(\\" ^ s ^ "." ^ string_of_expr a ^ ")"

let freevar e =
  let bound = H.create 4 in
  let found = H.create 4 in
  let ans = ref [] in
  let rec freevar' = function
    | Var s         -> if not (H.mem bound s) && not (H.mem found s)
                       then begin ans := s :: !ans; H.add found s (); end
                       else ()
    | App (a, b)    -> begin freevar' a; freevar' b; end
    | Lambda (s, a) -> begin H.add bound s (); freevar' a; H.remove bound s; end
  in freevar' e; List.sort compare !ans
