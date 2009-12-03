/* Dealing with symbols and environments. */

CAMLprim value r_install (value symbol) {
  /* The install function is defined in names.c. It basically looks up
     a symbol in the symbol table. If not found, it creates an entry in
     the symbol table. This is therefore an operation that has side-effects,
     but entry points in libR.so which would allow to implement a side-
     effect-free equivalent are hidden. */
  CAMLparam1(symbol);
  CAMLreturn(Val_sexp(install(String_val(symbol))));
}

CAMLprim value r_findvar (value symbol) {
  /* The findVar function is defined in envir.c. It looks up a symbol
     in an environment. */
  CAMLparam1(symbol);
  CAMLreturn(Val_sexp(findVar(Sexp_val(symbol), R_GlobalEnv)));
}

CAMLprim value r_findfun (value symbol) {
  /* This is essentially the same as findvar, with dynamic type checking
     for function types built into the R source code. The major difference
     is that findvar yields a promise, whether findfun yiels directly
     an R function sexptype. */
  CAMLparam1(symbol);
  CAMLreturn(Val_sexp(findFun(Sexp_val(symbol), R_GlobalEnv)));
}
