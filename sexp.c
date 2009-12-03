/* The NULL constant, and others, in R... */

CAMLprim value r_null (value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_sexp(R_NilValue));
}

CAMLprim value r_dots_symbol (value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_sexp(R_DotsSymbol));
}

CAMLprim value r_missing_arg (value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_sexp(R_MissingArg));
}


/* Comparison operator. */
CAMLprim value r_sexp_equality (value s1, value s2) {
  CAMLparam2(s1, s2);
  CAMLreturn(Val_bool(Sexp_val(s1) == Sexp_val(s2)));
}


/* Extracting runtime R low-level type information. */

CAMLprim value r_sexptype_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(TYPEOF(Sexp_val(sexp))));
}

