CAMLprim value ocamlr_write_lisplist_carval (value lisplist, value elmnt) {
  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
  return Val_unit;
}

CAMLprim value ocamlr_write_lisplist_tagval (value lisplist, value tag) {
  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
  return Val_unit;
}

//CAMLprim value r_write_lisplist_element (value lisplist, value tag, value elmnt) {
//  CAMLparam3(lisplist, tag, elmnt);
//  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
//  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
//  CAMLreturn(Val_unit);
//}


/**  Sets the element of a logical vector.
  *
  *  ocamlr_assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  */

CAMLprim value ocamlr_assign_lgl_vecsxp (value lglsxp, value offset, value b) {
  LOGICAL((int *) Vecsexp_val(lglsxp))[Int_val(offset)] = Bool_val(b);
  return Val_unit;
}


/**  Sets the element of a vector of integers.
  *
  *  ocamlr_assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  */

CAMLprim value ocamlr_assign_int_vecsxp (value intsxp, value offset, value i) {
  INTEGER((int *) Vecsexp_val(intsxp))[Int_val(offset)] = Int_val(i);
  return Val_unit;
}


/**  Sets the element of a vector of real numbers.
  *
  *  ocamlr_assign_real_vecsxp takes a vector of real numbers as first argument,
  *  an offset as second argument, and a real number as third argument,
  *  and sets the vector's offset element to the real number's value.
  */

CAMLprim value ocamlr_assign_real_vecsxp (value realsxp, value offset, value x) {
  REAL((double *) Vecsexp_val(realsxp))[Int_val(offset)] = Double_val(x);
  return Val_unit;
}


/**  Sets the element of a vector of string.
  *
  *  ocamlr_assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  */

CAMLprim value ocamlr_assign_str_vecsxp (value strsxp, value offset, value s) {
  STRING_PTR((int *) Vecsexp_val(strsxp))[Int_val(offset)] = mkChar(String_val(s));
  return Val_unit;
}
