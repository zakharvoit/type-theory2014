open Util
open Lambda

let _ = let (l, a, b) = read_lambda_with_assign stdin in
        print_endline (string_of_expr (substitute l a b))
