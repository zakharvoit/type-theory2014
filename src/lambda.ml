module H = Hashtbl

type expr = Lambda of string * expr
          | App of expr * expr
          | Var of string

type de_bruijn = DLambda of de_bruijn
               | DApp of de_bruijn * de_bruijn
               | DVar of int


let rec string_of_expr = function
  | Var s         -> s
  | App (a, b)    -> "(" ^ string_of_expr a ^ " " ^ string_of_expr b ^ ")"
  | Lambda (s, a) -> "(\\" ^ s ^ "." ^ string_of_expr a ^ ")"

let rec string_of_de_brujin = function
  | DVar x      -> string_of_int x
  | DApp (a, b) -> "(" ^ string_of_de_brujin a ^ " "
                  ^ string_of_de_brujin b ^ ")";
  | DLambda a   -> "(\\" ^ string_of_de_brujin a ^ ")"

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

let to_de_bruijn e =
  let depth = H.create 4 in
  List.iteri (fun i x -> H.add depth x (-i)) (freevar e);
  let rec convert d = function
    | Var s         -> DVar (d - H.find depth s)
    | Lambda (s, x) -> begin H.add depth s (d + 1);
                             let r = convert (d + 1) x in
                             H.remove depth s;
                             DLambda r
                       end
    | App (a, b)   -> DApp (convert d a, convert d b)
  in convert 0 e

let get_next_name idx =
  if idx = 0 then "a"
  else
    (* Generate number in the numeric system with radix 26 *)
    let rec generate = function
      | 0 -> []
      | x -> Char.chr (Char.code 'a' + x mod 26) :: generate (x / 26)
    in Str.of_list (generate idx)

let from_de_brujin e =
  let name = H.create 4 in
  let nid = ref 0 in
  let gen_name idx = if H.mem name idx
                     then H.find name idx
                     else let s = get_next_name !nid in
                          nid := !nid + 1;
                          H.add name idx s;
                          s
  in
  let rec convert d = function
    | DVar i      -> let idx = d - i in
                     let name = gen_name idx in
                     Var name
    | DApp (a, b)  -> App (convert d a, convert d b)
    | DLambda e    -> Lambda (gen_name (d + 1), convert (d + 1) e)
  in convert 0 e

let rec in_normal_form_db = function
  | DApp (DLambda _, _) -> false
  | DApp (a, b)         -> in_normal_form_db a && in_normal_form_db b
  | DLambda e           -> in_normal_form_db e
  | _                   -> true

let substitute_db e i b = match e with
  | Var x when x = i -> b
  | _ -> failwith "Stub"

let rec substitute l a e = match l with
  | Var s         when s = a -> e
  | Lambda (s, b) when s = a -> Lambda (s, b)
  | Lambda (s, b)            -> Lambda (s, substitute b a e)
  | App (b, c)               -> App (substitute b a e, substitute c a e)
  | o                        -> o
