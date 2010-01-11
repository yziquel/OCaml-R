val bool : bool -> bool t
(**  Converts an Objective Caml boolean value to an R boolean value,
  *  that is a mono-element array of booleans.
  *) 

val floats_of_t : float list t -> float list
(**  Converts an R array of real numbers into a list of Objective
  *  Caml floats.
  *)

val float_of_t : float t -> float
(**  Converts an R array of floats with one element into an Objective
  *  Caml float.
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
