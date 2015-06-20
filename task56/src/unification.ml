open Term

let rec iter f x = match f x with
  | Some x' -> iter f x'
  | None    -> x

let rec contains x = function
  | (Var y)       -> y = x
  | (Func (f, a)) -> List.fold_left (||) false (List.map (contains x) a)

let rec substitute x t = function
  | (Var y) when y = x -> t
  | (Func (f, a))      -> Func (f, List.map (substitute x t) a)
  | a                  -> a

let substitute_pair x t p = (substitute x t (fst p), substitute x t (snd p))

let is_important = function
  | (Var a, Var b)       -> a <> b
  | (Var a, Func (f, b)) -> true
  | _                    -> false

exception No_solution
exception Already_solved
let unification (equations : (term * term) list) =
  let try_eq all = function
    | (Func (f, a), Var x)
      -> let all' = List.filter ((<>) (Func (f, a), Var x)) all in
       (Var x, Func (f, a)) :: all'
    | (Var a, Var b) when a = b
      -> List.filter ((<>) (Var a, Var b)) all
    | (Func (f, a), Func (g, b))
      -> if f = g
	 then List.combine a b @ all
	 else raise No_solution
    | (Var x, t) ->
       if contains x t
       then raise No_solution
       else let all' = List.filter ((<>) (Var x, t)) all in
	    (Var x, t) :: List.map (substitute_pair x t) all'
  in
  let step s =
    let rec process all = function
      | []      -> all
      | x :: xs -> let all' = try_eq all x in
		   process all' xs
    in let s' = List.sort_uniq compare (process s s) in
       if s' = s then None
       else Some s'
  in List.filter is_important (iter step (List.sort_uniq compare equations))
