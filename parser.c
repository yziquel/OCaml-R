/* Parsing R code. */

//CAMLprim value parse_sexp (value s) {
//  CAMLparam1(s);
//  SEXP text ;
//  SEXP pr ;
//  ParseStatus status;
//  PROTECT(text = mkString(String_val(s)));
//  PROTECT(pr=R_ParseVector(text, 1, &status, R_NilValue));
//  UNPROTECT(2);
//  switch (status) {
//    case PARSE_OK:
//     break;
//    case PARSE_INCOMPLETE:
//    case PARSE_EOF:
//      caml_raise_with_string(*caml_named_value("Parse_incomplete"), (String_val(s)));
//    case PARSE_NULL:
//    case PARSE_ERROR:
//      caml_raise_with_string(*caml_named_value("Parse_error"), (String_val(s)));
//      }
//  CAMLreturn(Val_sexp(VECTOR_ELT(pr,0)));
//}

CAMLprim value r_parse_string (value expression, value n_to_eval) {
  CAMLparam1(expression);
  CAMLlocal1(parse_result);
  ParseStatus status;
  SEXP text;
  PROTECT(text = mkString(String_val(s)));

  /* Concerning the last and fourth argument of R_ParseVector:
     http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1835.html */

  parse_result = Val_sexp(R_ParseVector(text, Int_val(n_to_eval), &status, R_NilValue));
  UNPROTECT(1);
  value result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(status));
  Store_field(result, 1, parse_result);
  CAMLreturn(result);
}
