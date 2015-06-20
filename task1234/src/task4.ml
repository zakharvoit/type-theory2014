open Lambda
open Util
open Option

let _ = let l = read_lambda stdin in
        match to_normal_form l with
        | Some e -> print_endline (string_of_expr e)
        | None   -> print_endline "Нет нормальной формы"
