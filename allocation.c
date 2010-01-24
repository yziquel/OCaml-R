/* Memory allocation is done via functions exported from memory.c. */

/* TODO: PROTECT and UNPROTECT make sense if there is a risk that the
   object gets garbage-collected within the function. As such, many
   PROTECT and UNPROTECT macros are unnecessary. To protect the R value
   while we're back into OCaml code, we should use PreserveObject and
   ReleaseObject, but that should be done in Val_sexp. */


/**  Allocates a pairlist.
  *
  *  r_alloc_list takes an integer i as argument, and returns an R
  *  pairlist, also called a LISTSXP, of size i.
  *
  *  Such a function is necessary in order to avoid the tail
  *  recursion issue. While doing 'let x = f () in x::(aux y)' is
  *  an acceptable way to make aux tail recurse in Objective Caml,
  *  this relies on the way Objective Caml constructs list with
  *  the :: constructor. This feature is not available when we use
  *  stub code around the CONS macro for R. Allocating a list and
  *  filling it in imperatively is a workaround.
  */

CAMLprim value r_alloc_list (value i) {
  CAMLparam1(i);
  CAMLlocal1(result);
  /* allocList is a macro wrapping up a call to the
     Rf_allocList symbol from libR.so. */
  SEXP s;
  PROTECT(s = allocList(Int_val(i)));
  result = Val_sexp(s);
  UNPROTECT(1);
  CAMLreturn(result);
}


/**  Allocates a logical vector.
  *
  *  r_alloc_lgl_vector takes an integer i as argument, and returns
  *  an R vector of logical values of size i.
  */

CAMLprim value r_alloc_lgl_vector (value i) {
  CAMLparam1(i);
  CAMLlocal1(result);
  SEXP s;
  PROTECT(s = allocVector(LGLSXP, Int_val(i)));
  result = Val_sexp(s);
  UNPROTECT(1);
  CAMLreturn(result);
}


/**  Allocates a vector of integers.
  *
  *  r_alloc_int_vector takes an integer i as argument, and returns
  *  an R vector of integer values of size i.
  */

CAMLprim value r_alloc_int_vector (value i) {
  CAMLparam1(i);
  CAMLlocal1(result);
  SEXP s;
  PROTECT(s = allocVector(INTSXP, Int_val(i)));
  result = Val_sexp(s);
  UNPROTECT(1);
  CAMLreturn(result);
}


/**  Allocates a vector of real numbers.
  *
  *  r_alloc_real_vector takes an integer i as argument, and returns
  *  an R vector of i strings.
  */

CAMLprim value r_alloc_real_vector (value i) {
  CAMLparam1(i);
  CAMLlocal1(result);
  SEXP s;
  PROTECT(s = allocVector(REALSXP, Int_val(i)));
  result = Val_sexp(s);
  UNPROTECT(1);
  CAMLreturn(result);
}


/**  Allocates a vector of strings.
  *
  *  r_alloc_str_vector takes an integer i as argument, and returns
  *  an R vector of i strings.
  */

CAMLprim value r_alloc_str_vector (value i) {
  CAMLparam1(i);
  CAMLlocal1(result);
  SEXP s;
  PROTECT(s = allocVector(STRSXP, Int_val(i)));
  result = Val_sexp(s);
  UNPROTECT(1);
  CAMLreturn(result);
}
