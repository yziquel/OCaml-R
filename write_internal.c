CAMLprim value r_write_lisplist_element (value lisplist, value tag, value elmnt) {
  CAMLparam3(lisplist, tag, elmnt);
  Sexp_val(lisplist)->u.listsxp.tagval = Sexp_val(tag);
  Sexp_val(lisplist)->u.listsxp.carval = Sexp_val(elmnt);
  CAMLreturn(Val_unit);
}

