external write_listsxp_carval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_carval"
external write_listsxp_tagval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_tagval"

let write_listsxp_element l tag elmnt =
  let () = write_listsxp_tagval l tag in
  let () = write_listsxp_carval l elmnt in
  ()