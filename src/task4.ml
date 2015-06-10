open Lambda
open Util

let _ = let l = read_lambda stdin in
        let d = to_de_bruijn l in
        print_endline (string_of_de_brujin d)
