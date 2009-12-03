/* For the purpose of reverse-engineering, here's the stub for eval.
   I'll drop it afterwards, since R_tryEval seems the only solution
   to forward R errors to OCaml. */

CAMLprim value r_reveng_eval_sxp (value call) {
  CAMLparam1(call);
  CAMLreturn(Val_sexp(Rf_eval(Sexp_val(call), R_GlobalEnv)));
}

SEXP Rf_promiseArgs (SEXP el, SEXP rho);
CAMLprim value r_reveng_promise_args (value args) {
  CAMLparam1(args);
  CAMLreturn(Val_sexp(Rf_promiseArgs(Sexp_val(args), R_GlobalEnv)));
}

SEXP Rf_matchArgs (SEXP formals, SEXP supplied, SEXP call);
CAMLprim value r_reveng_match_args (value formals, value supplied, value call) {
  CAMLparam3(formals, supplied, call);
  CAMLreturn(Val_sexp(Rf_matchArgs(Sexp_val(formals), Sexp_val(supplied), Sexp_val(call))));
}

SEXP Rf_NewEnvironment (SEXP namelist, SEXP valuelist, SEXP rho);
CAMLprim value r_reveng_new_environment (value namelist, value valuelist, value rho) {
  CAMLparam3(namelist, valuelist, rho);
  CAMLreturn(Val_sexp(Rf_NewEnvironment(Sexp_val(namelist), Sexp_val(valuelist), Sexp_val(rho))));
}

SEXP Rf_mkPROMISE (SEXP expr, SEXP rho);
CAMLprim value r_reveng_mkPROMISE (value expr, value rho) {
  CAMLparam2(expr, rho);
  CAMLreturn(Val_sexp(Rf_mkPROMISE(Sexp_val(expr), Sexp_val(rho))));
}

void (SET_MISSING) (SEXP x, int v);
CAMLprim value r_reveng_SET_MISSING (value x, value v) {
  CAMLparam2(x, v);
  (SET_MISSING) (Sexp_val(x), Int_val(v));
  CAMLreturn(Val_unit);
}