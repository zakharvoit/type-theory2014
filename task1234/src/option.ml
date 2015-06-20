type 'x option = Some of 'x
               | None

let map f = function
  | Some x -> Some (f x)
  | None   -> None
