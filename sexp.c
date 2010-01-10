/* The NULL constant, and others, in R... */

CAMLprim value r_null (value unit) {
  return Val_sexp(R_NilValue);
}

CAMLprim value r_dots_symbol (value unit) {
  return Val_sexp(R_DotsSymbol);
}

CAMLprim value r_missing_arg (value unit) {
  return Val_sexp(R_MissingArg);
}

CAMLprim value r_base_env (value unit) {
  return Val_sexp(R_BaseEnv);
}

CAMLprim value r_global_env (value unit) {
  return Val_sexp(R_GlobalEnv);
}


/* Comparison operator. */
CAMLprim value r_sexp_equality (value s1, value s2) {
  return Val_bool(Sexp_val(s1) == Sexp_val(s2));
}


/* Extracting runtime R low-level type information. */

CAMLprim value r_sexptype_of_sexp (value sexp) {
  return Val_int(TYPEOF(Sexp_val(sexp)));
}

