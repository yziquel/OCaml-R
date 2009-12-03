external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval_string s = eval_langsxp (parse_sexp s)

let eval (l : sexp list) : sexp =
    (* The typing of l needs to be amended and moved to 'a t stuff. *)
    eval_langsxp (langsxp_of_list l (List.length l))

let mlfun (f: ('a -> 'b) t) (x: 'a t) : 'b t = eval [f; x]