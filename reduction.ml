external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval_string s = eval_langsxp (parse_sexp s)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f ?name x = Some (name, (f x))
let opt f name x = match x with
  | None -> None
  | Some xx -> Some ((Some name), (f xx))

let eval phi (args: (string option * sexp) list) =
  eval_langsxp (langsxp phi args)