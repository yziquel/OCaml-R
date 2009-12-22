val eval_string : string -> sexp

val arg : ('a -> 'b sxp) -> ?name:string -> 'a -> (string option * sexp) option
val opt : ('a -> 'b sxp) -> string -> 'a option -> (string option * sexp) option

val eval : sexp -> (string option * sexp) option list -> sexp

