/* Memory allocation is done via functions exported from memory.c. */

/**  Allocates a pairlist.
  *
  *  @note With 'let x = f () in x::(aux y)', you have tail recursion
  *        because of the way the :: constructor is used in OCaml. You
  *        cannot have tail-recursion with R CONS and the same code.
  *        Hence the need to pre-allocate with this function.
  *  @param i Size of the pairlist to allocate.
  *  @return A newly allocated pairlist.
  */
CAMLprim value ocamlr_alloc_list (value i) {
  /* allocList is a macro wrapping up a call to the
     Rf_allocList symbol from libR.so. */
  return(Val_sexp(allocList(Int_val(i))));
}


/**  Allocates a logical vector.
  *
  *  @param i Size of the logical vector to allocate.
  *  @return A newly allocated logical vector.
  */
CAMLprim value ocamlr_alloc_lgl_vector (value i) {
  return(Val_sexp(allocVector(LGLSXP, Int_val(i))));
}


/**  Allocates a vector of integers.
  *
  *  @param i Size of the vector of integers to allocate.
  *  @return A newly allocated vector of integers.
  */
CAMLprim value ocamlr_alloc_int_vector (value i) {
  return(Val_sexp(allocVector(INTSXP, Int_val(i))));
}


/**  Allocates a vector of real numbers.
  *
  *  @param i Size of the vector of real numbers to allocate.
  *  @return A newly allocated vector of real numbers.
  */
CAMLprim value ocamlr_alloc_real_vector (value i) {
  return(Val_sexp(allocVector(REALSXP, Int_val(i))));
}


/**  Allocates a vector of strings.
  *
  *  @param i Size of the vector of strings to allocate.
  *  @return A newly allocated vector of strings.
  */
CAMLprim value ocamlr_alloc_str_vector (value i) {
  return(Val_sexp(allocVector(STRSXP, Int_val(i))));
}
