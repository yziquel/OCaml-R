/* Data conversion to and from OCaml and R. */


/**  Cons operation of pairlists.
  *
  *  r_cons takes a SEXP car and a pairlist tail, and creates a
  *  new pairlist by consing car to tail.
  */

CAMLprim value r_cons (value car, value tail) {
  CAMLparam2(car, tail);
  CAMLlocal1(result)
  SEXP s = CONS(Sexp_val(car), Sexp_val(tail));
  result = Val_sexp(s);
  CAMLreturn(result);
}


/**  Tagging a pairlist's head.
  *
  *  r_tag takes a pairlist's head element, s, and a string t,
  *  and tags the pairlist's head with tag t.
  *
  *  This function is specifically used when constructing a
  *  LANGSXP for evaluation with named arguments.
  */

CAMLprim value r_tag (value s, value t) {
  CAMLparam2(s, t);
  /* Oh my! Oh my! this is unfortunately so ugly! *&+%*! R API! */
  SET_TAG(Sexp_val(s), install(String_val(t)));
  CAMLreturn(Val_unit);
}


/**  Creating a call out of a pairlist.
  *
  *  r_set_langsxp takes a pairlist as argument, and modifies the
  *  sexptype of the head of the list to be a LANGSXP. This allows
  *  the pairlist to be considered an executable call by the R
  *  interpreter.
  */

CAMLprim value r_set_langsxp (value s) {
  CAMLparam1(s);
  SET_TYPEOF(Sexp_val(s), LANGSXP);
  CAMLreturn(Val_unit);  
}


/**  Creates a string out of an internal CHARSXP structure.
  *
  *  r_internal_string_of_charsxp takes a CHARSXP as argument, and
  *  returns the string it contains.
  *
  *  When using the R API, this issues a function call. When using
  *  the #define USE_RINTERNALS directive, it is more efficient.
  */

CAMLprim value r_internal_string_of_charsxp (value charsxp) {
  CAMLparam1(charsxp);
  CAMLlocal1(result);

  /* Maxence Guesdon declares something like CAMLlocal1(result) for
     the output of caml_copy_string. To which extent is it necessary? */

  /* Moreover, it is yet unclear whether or not R strings are NULL
     terminated or size-delimited. I was told that R produces NULL
     terminated strings, but I haven't managed to get information
     on whether I should expect size-delimited strings originating
     from somewhere else... */

  result = caml_copy_string(CHAR(Sexp_val(charsxp)));
  CAMLreturn(result);
}


/**  Creates a CHARSXP out of a string.
  *
  *  r_charsxp_of_string takes an Objective Caml string s as
  *  argument, and returns a CHARSXP of this string.
  */

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
