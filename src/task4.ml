open Lambda
open Util

let _ = let l = read_lambda stdin in
        let d = to_de_bruijn l in
        let l' = from_de_brujin d in
        print_endline (string_of_expr l);
        print_endline (string_of_de_brujin d);
        print_endline (string_of_expr l')
