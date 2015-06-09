open Util
open Lambda

let _ = let l = read_lambda stdin in
        List.iter print_endline (freevar l)
