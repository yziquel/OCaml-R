external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval_string s = eval_langsxp (parse_sexp s)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f x = Some (f x)
let opt f x = match x with
  | None -> None
  | Some xx -> Some (f xx)

let eval (l : sexp list) : sexp =
    (* The typing of l needs to be amended and moved to 'a t stuff. *)
    eval_langsxp (langsxp_of_list l (List.length l))

let eval phi args =
  let l = phi::(prepare_args args) in
  eval_langsxp (langsxp_of_list l (List.length l))