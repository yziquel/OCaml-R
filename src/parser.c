/* Parsing R code. */


/**  Parses a given string.
  *
  *  @param expression A string containing R code, textually.
  *  @param n_to_eval How many calls to parse. If set to -1, stops
  *         when the string to parse is parsed completely.
  *  @return An integer error code, and an R vector, of type EXPRSXP,
  *          containing the parsing result.
  */
CAMLprim value ocamlr_parse_string (value expression, value n_to_eval) {
  CAMLparam1(expression);
  CAMLlocal1(parse_result);
  ParseStatus status;
  SEXP text;
  PROTECT(text = mkString(String_val(expression)));

  /* Concerning the last and fourth argument of R_ParseVector:
     http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1835.html */

  parse_result = Val_sexp(R_ParseVector(text, Int_val(n_to_eval), &status, R_NilValue));
  UNPROTECT(1);
  value result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(status));
  Store_field(result, 1, parse_result);
  CAMLreturn(result);
}
