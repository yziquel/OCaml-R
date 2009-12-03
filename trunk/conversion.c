/* Data conversion to and from OCaml and R. */

CAMLprim value r_cons (value car, value tail) {
  CAMLparam2(car, tail);
  SEXP s = CONS(Sexp_val(car), Sexp_val(tail));
  CAMLreturn(Val_sexp(s));
}

CAMLprim value r_tag (value s, value t) {
  CAMLparam2(s, t);
  /* Oh my! Oh my! this is unfortunately so ugly! *&+%*! R API! */
  SET_TAG(Sexp_val(s), install(String_val(s)));
  CAMLreturn(Val_unit);
}

CAMLprim value r_set_langsxp (value s) {
  CAMLparam1(s);
  SET_TYPEOF(Sexp_val(s), LANGSXP);
  CAMLreturn(Val_unit);  
}

CAMLprim value r_internal_string_of_charsxp (value charsxp) {
  CAMLparam1(charsxp);
  /* Maxence Guesdon declares something like CAMLlocal1(result) for
     the output of caml_copy_string. To which extent is it necessary? */
  /* Moreover, it is yet unclear whether or not R strings are NULL
     terminated, or if they simply have a size fixed in the VECSEXP structure. */
  CAMLreturn(caml_copy_string(CHAR(Sexp_val(charsxp))));
}

CAMLprim value r_charsxp_of_string (value s) {
  /* Documentation for generating R strings can be found in "Writing R
     extensions", section 5.9.7 "Handling character data". */
  CAMLparam1(s);
  SEXP charsxp;
  PROTECT(charsxp = mkChar(String_val(s)));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(charsxp));
}

CAMLprim value r_strsxp_of_string (value s) {
  CAMLparam1(s);
  SEXP strsxp;
  PROTECT(strsxp = mkString(String_val(s)));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(strsxp));
}
