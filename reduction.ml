(* The following exception needs to be registered
   in a callback when the R interpreter is initialised. *)
exception Runtime_error of lang sxp * string

external eval_langsxp : lang sxp -> 'a t = "r_eval_sxp"

let eval_string s = eval_langsxp (parse s)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f ?name x = Some (name, (Obj.magic (f x)))
let opt f name x = match x with
  | None -> None
  | Some x -> Some ((Some name), (Obj.magic (f x)))

let eval phi (args: (string option * sexp) option list) =
  eval_langsxp (langsxp phi (prepare_args args))
