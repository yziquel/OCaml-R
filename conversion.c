/* Data conversion to and from OCaml and R. */

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

/* r_langsxp_of_list creates a lisp-like list out of an
   OCaml list. langsxps, called pairlists, are not much
   used in R these days, except for internal matters,
   such as being an argument to R_tryEval.
   - Argument l is the OCaml list.
   - Argument n is the length of the OCaml list.
   Remark: LANGSXPs are essentially LISTSXPs with the
   type of the head element set to LANGSXP. */
CAMLprim value r_langsxp_of_list (value l, value n) {
  CAMLparam2(l, n);
  CAMLlocal1(l_cursor);
  SEXP s, t;

  PROTECT(t = s = Rf_allocList(Int_val(n)));
  SET_TYPEOF(s, LANGSXP);

  /* Fill in list s, with t moving over the pairlist,
     with values from l */
  int first_time = 1;
  l_cursor = l;
  while (l_cursor && Is_block(l_cursor)) {
    /* I've seen somewhere macros / functions allowing to dynamically
       allocate the next memory space of a listsxp. Will have to check
       that in more details later on. Would make code clearer. */
    if (first_time) {first_time = 0;} else {t = CDR(t);}
    SETCAR(t, Sexp_val(Field(l_cursor, 0)));
    l_cursor = Field(l_cursor, 1);
  }
  UNPROTECT(1);

  CAMLreturn(Val_sexp(s));
}

