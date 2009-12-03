/* Low-level data manipulation functions. */

/* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. The implementation
   details are in Rinternals.h, enclosed in #ifdef USE_RINTERNALS
   directives, making these opaque pointers when not turned on. */

/* Concerning VECSEXPs: */

/* Quotation from section 1.1.3 "The 'data'" of "R Internals" (R-ints.pdf):
   'This [i.e. vecsxp.truelength] is almost unused. The only current use is for
   hash tables of environments (VECSXPs), where length is the size of the table
   and truelength is the number of primary slots in use, and for the reference
   hash tables in serialization (VECSXPs),where truelength is the number of
   slots in use.' */

/* CHARSXPs are in fact VECSEXPs. Concerning encoding, quotation from section 1.1.2
   "Rest_of_header" of "R internals" (R-ints.pdf): 'As from R 2.5.0, bits 2 and 3 for
   a CHARSXP are used to note that it is known to be in Latin-1 and UTF-8 respectively.
   (These are not usually set if it is also known to be in ASCII, since code does not
   need to know the charset to handle ASCII strings. From R 2.8.0 it is guaranteed that
   they will not be set for CHARSXPs created by R itself.) As from R 2.8.0 bit 5 is used
   to indicate that a CHARSXP is hashed by its address, that is NA STRING or in the
   CHARSXP cache. */


CAMLprim value inspect_vecsxp_length (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLreturn(Val_int(Vecsexp_val(vecsexp)->vecsxp.length));
}

/*CAMLprim value inspect_vecsxp_truelength (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLreturn(Val_int(Vecsexp_val(vecsexp)->vecsxp.truelength));
}*/

/* Concerning various types of SEXPs: */

CAMLprim value inspect_primsxp_offset (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(Sexp_val(sexp)->u.primsxp.offset));
}

CAMLprim value inspect_symsxp_pname (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.pname));
}

CAMLprim value inspect_symsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.value));
}

CAMLprim value inspect_symsxp_internal (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.internal));
}

CAMLprim value inspect_listsxp_carval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.carval));
}

CAMLprim value inspect_listsxp_cdrval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.cdrval));
}

CAMLprim value inspect_listsxp_tagval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.tagval));
}

CAMLprim value inspect_envsxp_frame (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.frame));
}

CAMLprim value inspect_envsxp_enclos (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.enclos));
}

CAMLprim value inspect_envsxp_hashtab (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.hashtab));
}

CAMLprim value inspect_closxp_formals (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.formals));
}

CAMLprim value inspect_closxp_body (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.body));
}

CAMLprim value inspect_closxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.env));
}

CAMLprim value inspect_promsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.value));
}

CAMLprim value inspect_promsxp_expr (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.expr));
}

CAMLprim value inspect_promsxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.env));
}

CAMLprim value r_access_lgl_vecsxp (value lglsxp, value offset) {
  CAMLparam2(lglsxp, offset);
  CAMLreturn(Val_bool(LOGICAL((int *) Vecsexp_val(lglsxp))[Int_val(offset)]));
}

CAMLprim value r_access_int_vecsxp (value intsxp, value offset) {
  CAMLparam2(intsxp, offset);
  /* The R macro is #define INTEGER(x) ((int *) DATAPTR(x)).
     Should use Val_int, or int32s? More generally, the typing
     is here somewhat confusing (or confused)... Is offset an int? */
  CAMLreturn(Val_int(INTEGER((int *) Vecsexp_val(intsxp))[Int_val(offset)]));
}

CAMLprim value r_access_str_vecsxp (value strsxp, value offset) {
  CAMLparam2(strsxp, offset);
  /* Same comments as for r_accesas_int_vecsxp and for
     r_internal_string_of_charsxp. */
  CAMLreturn(caml_copy_string(CHAR(STRING_ELT(
    (char **) Vecsexp_val(strsxp), (Int_val(offset))))));
}

CAMLprim value r_access_sexp_vecsxp (value sexpsxp, value offset) {
  CAMLparam2(sexpsxp, offset);
  SEXP * v = (SEXP *) Vecsexp_val(sexpsxp);
  CAMLreturn(Val_sexp(VECTOR_ELT(v,Int_val(offset))));
}