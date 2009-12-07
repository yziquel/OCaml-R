/* Memory allocation is done via functions exported from memory.c. */


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
  result = Val_sexp(allocList(Int_val(i)));
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
  result = Val_sexp(allocVector(LGLSXP, Int_val(i)));
  CAMLreturn(result);
}
