external write_listsxp_carval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_carval"
external write_listsxp_tagval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_tagval"

let write_listsxp_element l tag elmnt =
  let () = write_listsxp_tagval l tag in
  let () = write_listsxp_carval l elmnt in
  ()

(**  Sets the element of a logical vector.
  *
  *  assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  *)

external assign_lgl_vecsxp  : vec_lgl  sxp -> int -> bool -> unit = "r_assign_lgl_vecsxp"


(**  Sets the element of a vector of integers.
  *
  *  assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  *)

external assign_int_vecsxp  : vec_int  sxp -> int -> int -> unit = "r_assign_int_vecsxp"


(**  Sets the element of a vector of string.
  *
  *  assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  *)

external assign_str_vecsxp  : vec_str  sxp -> int -> string -> unit = "r_assign_str_vecsxp"
