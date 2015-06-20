module U = Unification
module T = Term
module H = Hashtbl
open Lambda
open Util

let (-->) a b = T.Func ("->", [a; b])

let rec string_of_typeterm = function
  | T.Func ("->", [x; y]) -> "(" ^ string_of_typeterm x ^ " -> "
			     ^ string_of_typeterm y ^ ")"
  | T.Var x               -> x
  | a                     -> failwith "Not a type term"

let print_typeeq p =
  print_string (string_of_typeterm (fst p));
  print_string " = ";
  print_endline (string_of_typeterm (snd p))

let rec apply_subst s = function
  | T.Var x       -> begin
     try List.assoc (T.Var x) s
     with Not_found -> T.Var x
    end
  | T.Func (f, a) -> T.Func (f, List.map (apply_subst s) a)

let get_equations e =
  let current_type_id = ref 0 in
  let get_type _ = let res = "t" ^ get_next_name !current_type_id in
		   current_type_id := !current_type_id + 1;
		   T.Var res
  in
  let fv = freevar e in
  let context = H.create 32 in
  List.iter (fun x -> H.add context x (get_type ())) fv;
  let rec generate = function
    | Var x -> let t = H.find context x in
	       (t, [])
    | App (a, b) -> let (at, ae) = generate a in
		    let (bt, be) = generate b in
		    let t = get_type () in
		    (t, (at, bt --> t) :: ae @ be)
    | Lambda (s, a) -> let t = get_type () in
		       H.add context s t;
		       let (at, ae) = generate a in
		       H.remove context s;
		       (t --> at, ae)
  in let (t, eqs) = generate e in
     let c = ref [] in
     H.iter (fun k v -> c := (k, v) :: !c) context;
     (t, eqs, List.sort_uniq compare !c)

let _ = let l = read_lambda stdin in
	try
	  let (t, e, context) = get_equations l in
	  let subst = U.unification e in
	  let t' = apply_subst subst t in
	  print_endline (string_of_typeterm t');
	  List.iter (fun k -> print_endline (fst k ^ " : " ^ string_of_typeterm (snd k))) context
	with U.No_solution -> print_endline "Не имеет типа"
