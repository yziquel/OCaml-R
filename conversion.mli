val bool : bool -> bool t
(**  Converts an Objective Caml boolean value to an R boolean value,
  *  that is a mono-element array of booleans.
  *) 

val float_list_of_real_vecsxp : sexp -> float list
(**  Converts an R array of real numbers into a list of Objective
  *  Caml floats.
  *)

val strings_of_t : string list t -> string list
(**  Converts an R array of strings into a list of Objective Caml
  *  strings.
  *)

val string : string -> string t
(**  Converts an Objective Caml string to an R string, that is a
  *  mono-element array of strings.
  *)

val strings : string list -> string list t
(**  Converts an Objective Caml list of strings into an R array of
  *  strings.
  *)
