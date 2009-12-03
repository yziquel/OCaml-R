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
CAMLprim value r_reveng_SET_MISSING (value x, value v){
  CAMLparam2(expr, rho);
  (SET_MISSING) (Sexp_val(x), Int_val(v));
  CAMLreturn(Val_unit);
}

/*************************************************************************/





/********************************************/
/* For reverse-engineering purposes ONLY!!! */
/********************************************/

/* This is taken from memory.h */
static SEXPREC OCamlR_UnmarkedNode;

// Rinternals.h contains the macro definition:
// #define MARK(x)            ((x)->sxpinfo.mark)
//
//#define NODE_IS_MARKED(s) (MARK(s)==1)
//#define MARK_NODE(s) (MARK(s)=1)
//#define UNMARK_NODE(s) (MARK(s)=0)
//
// This is set in function InitMemory()...
//
// So we'll simply bother calling:

CAMLprim value r_init_ocaml_node (value unit) {
  CAMLparam1(unit);
  OCamlR_UnmarkedNode.sxpinfo.mark = 0;
  CAMLreturn(Val_unit);
}


/* Now the real code: */

CAMLprim value r_sexp_allocate (value unit) {
  /* We should be using allocSExp from memory.c instead. This would take
     care of all the stupid stuff up there concerning UnmarkedNode,
     garbage collection and tutti quanti. */
  CAMLparam1(unit);
  CAMLreturn(Val_sexp((SEXP) Calloc(1, SEXPREC)));
}

#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)       ((x)->u.promsxp.value)
#define PRSEEN(x)        ((x)->sxpinfo.gp)
CAMLprim value r_write_promise (value p, value expression) {
  CAMLparam2(p, expression);
  SEXP r_p = Sexp_val(p);
  r_p->sxpinfo = OCamlR_UnmarkedNode.sxpinfo;
  TYPEOF(r_p) = PROMSXP;
  PRCODE(r_p) = Sexp_val(expression);
  PRENV(r_p) = R_GlobalEnv;
  PRVALUE(r_p) = R_UnboundValue;
  PRSEEN(r_p) = 0;
  ATTRIB(r_p) = R_NilValue;
  CAMLreturn(Val_unit);
}
