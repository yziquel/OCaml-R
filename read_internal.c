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


/**  Returns the attributes of a SEXP or VECSEXP
  *
  *  inspect_attributes takes a SEXP or a VECSEXP as
  *  arguments and returns its attributes, as a SEXP.
  */

CAMLprim value inspect_attributes (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(ATTRIB(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the length of a VECSEXP.
  *
  *  inspect_vecsxp_length takes a VECSEXP as argument and
  *  returns its length, i.e., the number of elements.
  */

CAMLprim value inspect_vecsxp_length (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLlocal1(result);
  result = Val_int(LENGTH(Vecsexp_val(vecsexp)));
  CAMLreturn(result);
}


/*CAMLprim value inspect_vecsxp_truelength (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLlocal1(result);
  result = Val_int(TRUELENGTH(Vecsexp_val(vecsexp)));
  CAMLreturn(result);
}*/



/* Concerning various types of SEXPs: */


/**  Returns the offset at which the primitive can be found.
  *
  *  inspect_primsxp_offset take a SEXP denoting a function primitive
  *  as argument, and returns its offset in the table of primitives.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  */

CAMLprim value inspect_primsxp_offset (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_int(Sexp_val(sexp)->u.primsxp.offset);
  CAMLreturn(result);
}


/**  Returns the name of a symbol.
  *
  *  inspect_symsxp_pname take a SYMSXP as argument, and returns the
  *  SEXP containing its name.
  */

CAMLprim value inspect_symsxp_pname (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(PRINTNAME(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the value of a symbol.
  *
  *  inspect_symsxp_value takes a SYMXSP as argument, and returns the
  *  SEXP containing its value.
  */

CAMLprim value inspect_symsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(SYMVALUE(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the internal sexp of a symbol.
  *
  *  inspect_symsxp_internal takes a SYMSXP as argument, and returns the
  *  SEXP containing its internal value.
  */

CAMLprim value inspect_symsxp_internal (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(INTERNAL(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the head element of a pairlist.
  *
  *  inspect_listsxp_carval takes a pairlist as argument, and
  *  returns the SEXP containing its head element.
  */

CAMLprim value inspect_listsxp_carval (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(CAR(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the tail pairlist of a pairlist.
  *
  *  inspect_listsxp_cdrval takes a pairlist as argument,
  *  and returns its tail pairlist.
  */

CAMLprim value inspect_listsxp_cdrval (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(CDR(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the tag value of the head element of a pairlist.
  *
  *  inspect_listsxp_tagval takes a pairlist as argument, and
  *  returns the tag value of the head element of the pairlist.
  */

CAMLprim value inspect_listsxp_tagval (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(TAG(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the frame of an evironment.
  *
  *  inspect_envsxp_frame takes an environment as argument,
  *  and returns the frame of this environment.
  */

CAMLprim value inspect_envsxp_frame (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(FRAME(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the enclosing environment of an environment.
  *
  *  inspect_envsxp_enclos takes an environment as argument,
  *  and returns its enclosing environment.
  */

CAMLprim value inspect_envsxp_enclos (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(ENCLOS(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the hash table of an environment.
  *
  *  inspect_envsxp_hashtabl takes an environment as argument,
  *  and returns its hash table.
  */

CAMLprim value inspect_envsxp_hashtab (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(HASHTAB(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the list of formal arguments of a closure.
  *
  *  inspect_closxp_formals takes a closure as argument,
  *  and returns the list of its formal arguments.
  */

CAMLprim value inspect_closxp_formals (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(FORMALS(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the body of a closure.
  *
  *  inspect_closxp_body takes a closure as argument,
  *  and returns the body of this closure.
  */

CAMLprim value inspect_closxp_body (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(BODY(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the environment of a closure.
  *
  *  inspect_closxp_env takes a closure as argument,
  *  and returns the environment of this closure.
  */

CAMLprim value inspect_closxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(CLOENV(Sexp_val(sexp)));
  CAMLreturn(result);
}


/**  Returns the value of a promise.
  *
  *  inspect_promsxp_value takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  */

CAMLprim value inspect_promsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(Sexp_val(sexp)->u.promsxp.value);
  CAMLreturn(result);
}


/**  Returns the expression of a promise.
  *
  *  inspect_promsxp_expr takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  */

CAMLprim value inspect_promsxp_expr (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(Sexp_val(sexp)->u.promsxp.expr);
  CAMLreturn(result);
}


/**  Returns the environment of a promise.
  *
  *  inspect_promsxp_env takes a promise as argument,
  *  and returns the environment of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  */

CAMLprim value inspect_promsxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  result = Val_sexp(Sexp_val(sexp)->u.promsxp.env);
  CAMLreturn(result);
}


/**  Returns an element of a logical vector.
  *
  *  r_access_lgl_vecsxp takes a logical vector as argument,
  *  and an offset, and returns the element at this offset.
  */

CAMLprim value r_access_lgl_vecsxp (value lglsxp, value offset) {
  CAMLparam2(lglsxp, offset);
  CAMLlocal1(result);
  result = Val_bool(LOGICAL((int *) Vecsexp_val(lglsxp))[Int_val(offset)]);
  CAMLreturn(result);
}


/**  Returns an element of a vector of integers.
  *
  *  r_access_int_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  */

CAMLprim value r_access_int_vecsxp (value intsxp, value offset) {
  CAMLparam2(intsxp, offset);
  CAMLlocal1(result);

  /* The R macro is #define INTEGER(x) ((int *) DATAPTR(x)).
     Should use Val_int, or int32s? More generally, the typing
     is here somewhat confusing (or confused)... Is offset an int? */

  result = Val_int(INTEGER((int *) Vecsexp_val(intsxp))[Int_val(offset)]);;
  CAMLreturn(result);
}


/**  Returns an element of a vector of real numbers.
  *
  *  r_access_real_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  */

CAMLprim value r_access_real_vecsxp (value realsxp, value offset) {
  CAMLparam2(realsxp, offset);
  CAMLlocal1(result);
  result = caml_copy_double(REAL((double *) Vecsexp_val(realsxp))[Int_val(offset)]);
  CAMLreturn(result);
}


/**  Returns an element of a vector of strings.
  *
  *  r_access_str_vecsxp takes a vector of strings as argument,
  *  and an offset, and returns the element at this offset.
  */

CAMLprim value r_access_str_vecsxp (value strsxp, value offset) {
  CAMLparam2(strsxp, offset);
  CAMLlocal1(result);

  /* Same comments as for r_access_int_vecsxp and for
     r_internal_string_of_charsxp. */

  result = caml_copy_string(CHAR(STRING_ELT((char **)
    Vecsexp_val(strsxp), (Int_val(offset)))));
  CAMLreturn(result);
}


/**  Returns an element of a vector of SEXPs.
  *
  *  r_access_sexp_vecsxp takes a vector of SEXPs as argument,
  *  and an offset, and returns the element at this offset.
  */

CAMLprim value r_access_sexp_vecsxp (value sexpsxp, value offset) {
  CAMLparam2(sexpsxp, offset);
  CAMLlocal1(result);
  SEXP * v = (SEXP *) Vecsexp_val(sexpsxp);
  result = Val_sexp(VECTOR_ELT(v, Int_val(offset)));
  CAMLreturn(result);
}
