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


/**  Sets the element of a vector of integers.
  *
  *  r_assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  */

CAMLprim value r_assign_int_vecsxp (value intsxp, value offset, value i) {
  CAMLparam3(intsxp, offset, i);
  INTEGER((int *) Vecsexp_val(intsxp))[Int_val(offset)] = Int_val(i);
  CAMLreturn(Val_unit);
}


/**  Sets the element of a vector of string.
  *
  *  r_assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  */

CAMLprim value r_assign_str_vecsxp (value strsxp, value offset, value s) {
  CAMLparam3(strsxp, offset, s);
  SEXP charsxp;
  PROTECT(charsxp = mkChar(String_val(s)));
  UNPROTECT(1);
  STRING_PTR((int *) Vecsexp_val(strsxp))[Int_val(offset)] = charsxp;
  CAMLreturn(Val_unit);
}
