open Term
open Util

let _ = let (a, b) = parse_channel Parser.equation stdin in
	print_endline (string_of_term a);
	print_endline (string_of_term b)
