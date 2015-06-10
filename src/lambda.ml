module H = Hashtbl
open Option

let (|>) f g x = f (g x)

type expr = Lambda of string * expr
          | App of expr * expr
          | Var of string

type de_bruijn = DLambda of de_bruijn
               | DApp of de_bruijn * de_bruijn
               | DVar of int
               | DLazy of (unit -> de_bruijn)

let make_lazy f e =
  let r = ref None in
  fun _ -> match !r with
           | None -> let x = f e in r := Some x; x
           | Some re -> re

let rec string_of_expr = function
  | Var s         -> s
  | App (a, b)    -> "(" ^ string_of_expr a ^ " " ^ string_of_expr b ^ ")"
  | Lambda (s, a) -> "(\\" ^ s ^ "." ^ string_of_expr a ^ ")"

let rec string_of_de_bruijn = function
  | DVar x      -> string_of_int x
  | DApp (a, b) -> "(" ^ string_of_de_bruijn a ^ " "
                  ^ string_of_de_bruijn b ^ ")";
  | DLambda a   -> "(\\" ^ string_of_de_bruijn a ^ ")"
  | DLazy f     -> string_of_de_bruijn (f ())

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

(* Add constant to all free variables *)
let free_add d e =
  let bound = H.create 4 in
  let rec add depth = function
    | DVar a when not (H.mem bound (depth - a))
                  -> DVar (a + d)
    | DVar a      -> DVar a
    | DApp (a, b) -> DApp (add depth a, add depth b)
    | DLambda e   -> begin H.add bound (depth + 1) ();
                           let r = add (depth + 1) e in
                           H.remove bound (depth + 1);
                           DLambda r
                     end
    | DLazy f     -> DLazy (add depth |> f)
  in add 0 e

let from_de_bruijn e =
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
    | DLazy f      -> convert d (f ())
  in convert 0 e

let rec in_normal_form_db = function
  | DApp (DLambda _, _) -> false
  | DApp (a, b)         -> in_normal_form_db a && in_normal_form_db b
  | DLambda e           -> in_normal_form_db e
  | DLazy f             -> in_normal_form_db (f ())
  | _                   -> true

let substitute_db e i b =
  let rec subst i depth = function
    | DVar x when x = i -> free_add depth b
    | DVar a            -> DVar a
    | DLambda e         -> DLambda (subst (i + 1) (depth + 1) e)
    | DApp (p, q)       -> DApp (subst i depth p, subst i depth q)
    | DLazy f           -> DLazy ((subst i depth) |> f)
  in let r = subst i 1 e in free_add (-1) r

let rec b_reduction = function
  | DApp (DLambda a, b) -> (true, substitute_db a 0 (b_reduce b))
  | DApp (a, b)         -> let (f, e) = b_reduction a in
                           if f
                           then (true, DApp (e, b))
                           else let (f, e) = b_reduction b in
                                if f
                                then (true, DApp (a, e))
                                else (false, DApp (a, b))
  | DLambda a           -> let (f, e) = b_reduction a in
                           if f
                           then (true, DLambda e)
                           else (false, DLambda a)
  | DLazy f             -> b_reduction (f ())
  | a                   -> (false, a)
  and b_reduce e = let (_, r) = b_reduction e in r

let to_normal_form_db e =
  (* let used = H.create 4 in *)

  let rec convert e = (* if H.mem used e *)
                  (* then None *)
                  (* else *) let (f, e') = b_reduction e in
                       if f
                       then begin (* H.add used e (); *)
                                  convert e';
                            end
                       else if in_normal_form_db e'
                       then Some e'
                       else failwith "ASD"
  in convert e

let to_normal_form e =
  let d = to_de_bruijn e in
  let n = to_normal_form_db d in
  let r = map from_de_bruijn n in
  r

exception Not_free
let substitute l a e =
  let fv = freevar e in
  let bound = H.create 4 in
  let rec subst = function
    | Var s         when s = a -> begin
        if List.exists (H.mem bound) fv
        then raise Not_free
        else e
      end
    | Lambda (s, b) when s = a -> Lambda (s, b)
    | Lambda (s, b)            -> begin
        H.add bound s ();
        let r = Lambda (s, subst b) in
        H.remove bound s;
        r
      end
    | App (b, c)               -> App (subst b, subst c)
    | o                        -> o
  in subst l
