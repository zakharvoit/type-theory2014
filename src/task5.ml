open Term
open Util
open Unification

let _ =
  let rec read_equations _ = try let eq = parse_string Parser.equation (read_line ()) in
				 eq :: read_equations ()
			     with End_of_file -> []
  in
  let equations = read_equations () in
  try List.iter print_equation (unification equations)
  with No_solution -> print_endline "Нет решения"

