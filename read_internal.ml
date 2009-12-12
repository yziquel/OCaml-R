(* Low-level data manipulation functions. *)

(* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. *)


(**  Returns the attributes of a SEXP or VECSEXP
  *
  *  inspect_attributes takes a SEXP or a VECSEXP as
  *  arguments and returns its attributes, as a SEXP.
  *)

external inspect_attributes : sexp -> sexp = "inspect_attributes"


(**  Returns the length of a VECSEXP.
  *
  *  length_of_vecsxp takes a VECSEXP as argument and
  *  returns its length, i.e., the number of elements.
  *)

external length_of_vecsxp : 'a vecsxp -> int = "inspect_vecsxp_length"


(**  Returns the offset at the primitive can be found.
  *
  *  inspect_primsxp_offset take a SEXP denoting a function primitive
  *  as argument, and returns its offset in the table of primitives.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_primsxp_offset  : builtin sxp -> int = "inspect_primsxp_offset"


(**  Returns the name of a symbol.
  *
  *  inspect_symsxp_pname take a SYMSXP as argument, and returns the
  *  SEXP containing its name.
  *)

external inspect_symsxp_pname    : 'a sym sxp -> sexp = "inspect_symsxp_pname"


(**  Returns the value of a symbol.
  *
  *  inspect_symsxp_value takes a SYMXSP as argument, and returns the
  *  SEXP containing its value.
  *)

external inspect_symsxp_value    : 'a sym sxp -> sexp = "inspect_symsxp_value"


(**  Returns the internal sexp of a symbol.
  *
  *  inspect_symsxp_internal takes a SYMSXP as argument, and returns the
  *  SEXP containing its internal value.
  *)

external inspect_symsxp_internal : 'a sym sxp -> sexp = "inspect_symsxp_internal"


(**  Returns the head element of a pairlist.
  *
  *  inspect_listsxp_carval takes a pairlist as argument, and
  *  returns the SEXP containing its head element.
  *)

external inspect_listsxp_carval  : 'a lisplist sxp -> sexp = "inspect_listsxp_carval"


(**  Returns the tail pairlist of a pairlist.
  *
  *  inspect_listsxp_cdrval takes a pairlist as argument,
  *  and returns its tail pairlist.
  *)

external inspect_listsxp_cdrval  : 'a lisplist sxp -> sexp = "inspect_listsxp_cdrval"


(**  Returns the tag value of the head element of a pairlist.
  *
  *  inspect_listsxp_tagval takes a pairlist as argument, and
  *  returns the tag value of the head element of the pairlist.
  *)

external inspect_listsxp_tagval  : 'a lisplist sxp -> sexp = "inspect_listsxp_tagval"


(**  Returns the frame of an evironment.
  *
  *  inspect_envsxp_frame takes an environment as argument,
  *  and returns the frame of this environment.
  *)

external inspect_envsxp_frame    : env sxp -> sexp = "inspect_envsxp_frame"


(**  Returns the enclosing environment of an environment.
  *
  *  inspect_envsxp_enclos takes an environment as argument,
  *  and returns its enclosing environment.
  *)

external inspect_envsxp_enclos   : env sxp -> sexp = "inspect_envsxp_enclos"


(**  Returns the hash table of an environment.
  *
  *  inspect_envsxp_hashtabl takes an environment as argument,
  *  and returns its hash table.
  *)

external inspect_envsxp_hashtab  : env sxp -> sexp = "inspect_envsxp_hashtab"


(**  Returns the list of formal arguments of a closure.
  *
  *  inspect_closxp_formals takes a closure as argument,
  *  and returns the list of its formal arguments.
  *)

external inspect_closxp_formals  : clos sxp -> sexp = "inspect_closxp_formals"


(**  Returns the body of a closure.
  *
  *  inspect_closxp_body takes a closure as argument,
  *  and returns the body of this closure.
  *)

external inspect_closxp_body     : clos sxp -> sexp = "inspect_closxp_body"


(**  Returns the environment of a closure.
  *
  *  inspect_closxp_env takes a closure as argument,
  *  and returns the environment of this closure.
  *)

external inspect_closxp_env      : clos sxp -> sexp = "inspect_closxp_env"


(**  Returns the value of a promise.
  *
  *  inspect_promsxp_value takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_value   : prom sxp -> sexp = "inspect_promsxp_value"


(**  Returns the expression of a promise.
  *
  *  inspect_promsxp_expr takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_expr    : prom sxp -> sexp = "inspect_promsxp_expr"


(**  Returns the environment of a promise.
  *
  *  inspect_promsxp_env takes a promise as argument,
  *  and returns the environment of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_env     : prom sxp -> sexp = "inspect_promsxp_env"


(**  Returns an element of a logical vector.
  *
  *  access_lgl_vecsxp takes a logical vector as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_lgl_vecsxp  : vec_lgl  sxp -> int -> bool         = "r_access_lgl_vecsxp"


(**  Returns an element of a vector of integers.
  *
  *  access_int_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_int_vecsxp  : vec_int  sxp -> int -> int          = "r_access_int_vecsxp"


(**  Returns an element of a vector of real numbers.
  *
  *  access_real_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_real_vecsxp : vec_real sxp -> int -> float        = "r_access_real_vecsxp"


(**  Returns an element of a vector of strings.
  *
  *  access_str_vecsxp takes a vector of strings as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_str_vecsxp  : vec_str  sxp -> int -> string       = "r_access_str_vecsxp"


(**  Returns an element of a vector of SEXPs.
  *
  *  access_sexp_vecsxp takes a vector of SEXPs as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_sexp_vecsxp : vec_sexp sxp -> int -> sexp         = "r_access_sexp_vecsxp"
