/**********************************************************************
 *                                                                    *
 *                   Beta reduction of R calls.                       *
 *                                                                    *
 **********************************************************************/


CAMLprim value r_eval_sxp (value sexp_list) {

  /* sexp_list is an OCaml value containing a SEXP of sexptype LANGSXP.
     This is a LISP-style pairlist of SEXP values. r_eval_sxp executes
     the whole string, and sends back the resulting SEXP wrapped up in
     an OCaml value. There's also an error handling mechanism.
     r_eval_sxp handles values of type LANGSXP and PROMSXP. So we have two
     functions on the OCaml side associated to this stub, the first on
     with type lang sexp -> raw sexp, the other one with type
     prom sexp -> raw sexp. This also means that their is a dynamic type
     checking being done in the scope of the R_tryEval function, and it
     would be nice to shortcut it with statically typed equivalents. */

  CAMLparam1(sexp_list);

  SEXP e;        // Placeholder for the result of beta-reduction.
  int error = 0; // Error catcher boolean.

  /* Should this be wrapped with a PROTECT() and
     an UNPROTECT(1), or not? */
  PROTECT(e = R_tryEval(Sexp_val(sexp_list), R_GlobalEnv, &error));
  UNPROTECT(1);

  /* Will have to implement error handling in OCaml. */
  if (error) caml_failwith("OCaml-R error in r_eval_sxp C stub.");

  CAMLreturn(Val_sexp(e));
}

CAMLprim value r_apply_closure ( value call, value op, value arglist) {
  CAMLparam3(call, op, arglist);
  CAMLreturn(Val_sexp(Rf_applyClosure(Sexp_val(call), Sexp_val(op),
    Sexp_val(arglist), R_GlobalEnv, R_BaseEnv)));
}

/**********************************************************************
 *                                                                    *
 *                  Execution of R expressions.                       *
 *                                                                    *
 **********************************************************************/

/* The function below has been commented, because it should be a
   combination of a parsing function and of an eval function. */

//CAMLprim value r_sexp_of_string (value expression) {
//
//  /* This function makes use of the camlrtmp symbol. We'd like
//     to create a function with similar semantics which does not
//     populate the symbols table. */
//
//  CAMLparam1(expression);
//  char* c_name = "camlrtmp";
//  char* s_exp;
//  CAMLlocal1(result);
//  SEXP e, tmp;
//  int hadError;
//  ParseStatus status;
//
//  asprintf(&s_exp, "%s = %s", c_name, String_val(expression));
//  PROTECT(tmp = mkString(s_exp));
//  PROTECT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
//  /* PrintValue(e); DEBUG */
//  R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
//  UNPROTECT(2);
//  free(s_exp);
//  result = r_sexp_of_symbol(caml_copy_string(c_name));
//  CAMLreturn(result);
//}
