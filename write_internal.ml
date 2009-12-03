external write_lisplist_carval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_carval"
external write_lisplist_tagval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_tagval"

let write_lisplist_element l tag elmnt =
  let () = write_lisplist_tagval l tag in
  let () = write_lisplist_carval l elmnt in
  ()