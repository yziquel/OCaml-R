CAMLprim value r_write_lisplist_carval (value lisplist, value elmnt) {
  CAMLparam2(lisplist, elmnt);
  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
  CAMLreturn(Val_unit);
}

CAMLprim value r_write_lisplist_tagval (value lisplist, value tag) {
  CAMLparam2(lisplist, tag);
  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
  CAMLreturn(Val_unit);
}

//CAMLprim value r_write_lisplist_element (value lisplist, value tag, value elmnt) {
//  CAMLparam3(lisplist, tag, elmnt);
//  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
//  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
//  CAMLreturn(Val_unit);
//}


/**  Sets the element of a logical vector.
  *
  *  r_assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  */

CAMLprim value r_assign_lgl_vecsxp (value lglsxp, value offset, value b) {
  CAMLparam3(lglsxp, offset, b);
  LOGICAL((int *) Vecsexp_val(lglsxp))[Int_val(offset)] = Bool_val(b);
  CAMLreturn(Val_unit);
}

