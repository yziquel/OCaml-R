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

