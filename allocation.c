/* Memory allocation is done via functions exported from memory.c. */

CAMLprim value r_alloc_list (value i) {
  CAMLparam1(i);
  CAMLreturn(Val_sexp(Rf_allocList(Int_val(i))));
}

