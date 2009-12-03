/* Wrapping and unwrapping of R values. */

CAMLprim value Val_sexp (SEXP sexp) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value) sexp;
    /* Do not use Val_long in the above statement,
       as it will drop the top bit. See mlvalues.h. */
  CAMLreturn(result);
}

SEXP Sexp_val (value sexp) {
  return (SEXP) Field(sexp, 0);
}

#define Val_vecsexp(x) Val_sexp(x)
#define Vecsexp_val(x) ((VECSEXP) Sexp_val(x))

