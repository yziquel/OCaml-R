CAMLprim value r_is_s4_object (value ml_s4) {
  CAMLparam1(ml_s4);
  CAMLlocal1(result);
  int b = IS_S4_OBJECT(Sexp_val(ml_s4));
  result = Val_bool(b);
  CAMLreturn(result);
}

CAMLprim value r_do_new_object (value ml_s4_class) {
  CAMLparam1(ml_s4_class);
  CAMLlocal1(result);
  SEXP new_object = R_do_new_object(Sexp_val(ml_s4_class));
  result = Val_sexp(new_object);
  CAMLreturn(result);
}
