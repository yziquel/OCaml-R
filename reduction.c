/* TODO: declare static what should be declared static... */



/**********************************************************************
 *                                                                    *
 *                 Error handling from R to OCaml                     *
 *                                                                    *
 **********************************************************************/


/* Transmitting errors from R to Objective Caml is a rather painful
   topic. Essentially because it is undocumented, but also not
   supported by the R API. And not even "public". So we have to resort
   to writing our own headers to cope with this. For discussion, see
   posting https://stat.ethz.ch/pipermail/r-help/2008-August/171493.html */

void R_SetErrorHook(void (*hook)(SEXP, char *));

/* This header allows us to access a function that sets hooks for error
   handling. Moreover, each time an error occurs, the hook is removed. So
   you have to re-hook the hook from code within the hook itself...
   Moreover, behaviour when coping recursively with errors is unknown. */

/* The global variables where we cache our error status. */

static SEXP error_call = NULL;
static char * error_message = NULL;

/* The hook in charge of caching the error status. */

static void r_error_hook(SEXP call, char * message);
static void r_error_hook(SEXP call, char * message) {
  error_call = call;
  error_message = message;
  R_SetErrorHook(&r_error_hook);
}

CAMLprim value r_init_error_hook (value ml_unit) {
  R_SetErrorHook(&r_error_hook);
  return Val_unit;
}


/**********************************************************************
 *                                                                    *
 *                   Beta reduction of R calls.                       *
 *                                                                    *
 **********************************************************************/


CAMLprim value r_eval_sxp (value sexp_list) {

  /* sexp_list is an OCaml value containing a SEXP of sexptype LANGSXP.
     This is a LISP-style pairlist of SEXP values. r_eval_sxp executes
     the whole pairlist, and sends back the resulting SEXP wrapped up in
     an OCaml value. There's also an error handling mechanism. */

  /* r_eval_sxp handles values of type LANGSXP and PROMSXP. So we have two
     functions on the OCaml side associated to this stub, the first on
     with type lang sexp -> raw sexp, the other one with type
     prom sexp -> raw sexp. This also means that there is a dynamic type
     checking being done in the scope of the R_tryEval function, and it
     would be nice to shortcut it with statically typed equivalents. */

  CAMLparam0();
  CAMLlocal2(ml_error_call, ml_error_message);

  SEXP e;        // Placeholder for the result of beta-reduction.
  int error = 0; // Error catcher boolean.

  /* Should this be wrapped with a PROTECT() and an UNPROTECT(1), or
     not? */
  enter_blocking_section();
  PROTECT(e = R_tryEval(Sexp_val(sexp_list), R_GlobalEnv, &error));
  UNPROTECT(1);
  leave_blocking_section();

  /* Implements error handling from R to Objective Caml. */
  if (error) {

    ml_error_call = Val_sexp(error_call);
    error_call = NULL;      //should check for a memory leak here...

    ml_error_message = caml_copy_string(error_message);
    error_message = NULL;   //should check for a memory leak here...

    value error_result = caml_alloc_small(2, 0);
    Store_field(error_result, 0, ml_error_call);
    Store_field(error_result, 1, ml_error_message);

    /* The exception callback mechanism is described on the webpage
       http://www.pps.jussieu.fr/Livres/ora/DA-OCAML/book-ora118.html
       We should check to see if we could the string-name lookup to
       avoid unnecessary delays in exception handling. */
    raise_with_arg(*caml_named_value("OCaml-R generic error"), error_result);
  }

  CAMLreturn(Val_sexp(e));
}

//CAMLprim value r_apply_closure (value call, value op, value arglist) {
//  CAMLparam3(call, op, arglist);
//  CAMLreturn(Val_sexp(Rf_applyClosure(Sexp_val(call), Sexp_val(op),
//    Sexp_val(arglist), R_GlobalEnv, R_BaseEnv)));
//}



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
