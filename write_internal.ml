open Sexptype

external write_lisplist_element : 'a lisplist sxp -> sexp -> sexp -> unit = "r_write_lisplist_element"
