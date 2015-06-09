open Util
open Lambda

let _ = let s = read_line () in
        let l = parse_lambda s in
        print_endline (string_of_expr l)
