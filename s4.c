CAMLprim value r_is_s4_object (value ml_s4) {
  return(Val_bool(IS_S4_OBJECT(Sexp_val(ml_s4))));
}

CAMLprim value r_do_new_object (value ml_s4_class) {
  return(Val_sexp(R_do_new_object(Sexp_val(ml_s4_class))));
}
