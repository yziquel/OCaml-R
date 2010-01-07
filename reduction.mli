val eval_string : string -> sexp
(**  [eval_string] take a string containing R code, and feeds it to the
  *  R interpreter. You get the resulting value back.
  *)

val arg : ('a -> 'b sxp) -> ?name:string -> 'a -> (string option * sexp) option
(**  Convenience function to wrap up arguments, when mapping R functions
  *  to Objective Caml functions.
  *)

val opt : ('a -> 'b sxp) -> string -> 'a option -> (string option * sexp) option
(**  Convenience function to wrap up optional arguments, when mapping R functions
  *  to Objective Caml functions.
  *)

val eval : sexp -> (string option * sexp) option list -> sexp
(**  Evaluates an R function given a list of arguments. *)

