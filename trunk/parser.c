/* Parsing R code. */

CAMLprim value parse_sexp (value s) {
  CAMLparam1(s);
  SEXP text ;
  SEXP pr ;
  ParseStatus status;
  PROTECT(text = mkString(String_val(s)));
  PROTECT(pr=R_ParseVector(text, 1, &status, R_NilValue));
  UNPROTECT(2);
  switch (status) {
    case PARSE_OK:
     break;
    case PARSE_INCOMPLETE:
    case PARSE_EOF:
      caml_raise_with_string(*caml_named_value("Parse_incomplete"), (String_val(s)));
    case PARSE_NULL:
    case PARSE_ERROR:
      caml_raise_with_string(*caml_named_value("Parse_error"), (String_val(s)));
      }
  CAMLreturn(Val_sexp(VECTOR_ELT(pr,0)));
}

