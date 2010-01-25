/**********************************************************************
 *                                                                    *
 *                 New code concerning symbols.                       *
 *                                                                    *
 **********************************************************************/

/* Dealing with symbols and environments. */

CAMLprim value r_install (value symbol) {

  /* The install function is defined in names.c. It basically looks up
     a symbol in the symbol table. If not found, it creates an entry in
     the symbol table. This is therefore an operation that has side-effects,
     but entry points in libR.so which would allow to implement a side-
     effect-free equivalent are hidden. */

  /* Remark: explanation of the install function at
     https://stat.ethz.ch/pipermail/r-devel/2009-August/054494.html */

  return(Val_sexp(install(String_val(symbol))));
}

CAMLprim value r_findvar (value symbol) {
  /* The findVar function is defined in envir.c. It looks up a symbol
     in an environment. */
  return(Val_sexp(findVar(Sexp_val(symbol), R_GlobalEnv)));
}

CAMLprim value r_findfun (value symbol) {
  /* This is essentially the same as findvar, with dynamic type checking
     for function types built into the R source code. The major difference
     is that findvar yields a promise, whether findfun yiels directly
     an R function sexptype. */
  return(Val_sexp(findFun(Sexp_val(symbol), R_GlobalEnv)));
}

/**********************************************************************
 *                                                                    *
 *                 Old code concerning symbols.                       *
 *                                                                    *
 **********************************************************************/

//CAMLprim value r_set_var (value name, value sexp) {
//  CAMLparam2(name,sexp);
//  char* c_name = String_val(name);
//  SEXP e = (SEXP) Long_val(Field(sexp,0));
//  setVar(install(c_name), e, R_GlobalEnv);
//  CAMLreturn(Val_unit);
//}
