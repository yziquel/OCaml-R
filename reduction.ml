external eval_string : string -> sexp = "r_sexp_of_string"

external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval (l : sexp list) : sexp =
    (* The typing of l needs to be amended and moved to 'a t stuff. *)
    eval_langsxp (langsxp_of_list l (List.length l))

external force_promsxp : prom sxp -> sexp = "r_eval_sxp"

let force : 'a promise -> 'a t = force_promsxp

let mlfun (f: ('a -> 'b) t) (x: 'a) : 'b t = eval [f; x]

