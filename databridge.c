/* Wrapping and unwrapping of R values. */

/* TODO: Abstract some low level wrapping to a dedicated .so (i.e. other project). */

/* TODO: Think of replacing caml_alloc by caml_alloc_small for efficiency
   reasons. However, one has to deal with GC interaction. See the posts
   http://www.mail-archive.com/caml-list@yquem.inria.fr/msg00725.html and
   http://www.mail-archive.com/caml-list@yquem.inria.fr/msg00726.html */

/* TODO: There are some uses of malloc that should be replaced by
   caml_stat_alloc, which is malloc, but raises the OCaml out of memory
   exception. See http://old.nabble.com/help-with-caml_stat_alloc-tc5463112.html */

/* TODO: To wrap strings, (which we do elsewhere), or more precisely, to
   convert an OCaml string to a C string, INRIA guys use the construct

          char * p;
          p = caml_stat_alloc(caml_string_length(path) + 1);
          strcpy(p, String_val(path));
          [...]
          caml_stat_free(p);

   We should investigate this construct. */

/* TODO: We are now using custom blocks for pointer to R SEXPs. We use values of
   0, 1 in caml_alloc_custom to calibrate the GC. This is obviously insufficient,
   as a SEXP might hold a huge amount of data, specifically in R. We should
   therefore think of writing a BigVal_sexp(SEXP sexp, mlsize_t mem, mlsize_t max)
   function, allowing us to taylor the behaviour of the GC, depending on data size. */

static void r_valsexp_finalisation (value valsexp) {
  R_ReleaseObject(*((SEXP *) Data_custom_val(valsexp)));
}

static struct custom_operations r_sexp_ops = {
  "org.homelinux.yziquel.OCaml-R",
  r_valsexp_finalisation,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value Val_sexp (SEXP sexp) {
  R_PreserveObject(sexp);  // registers the SEXP as an R GC root.
  value result = caml_alloc_custom(&r_sexp_ops, sizeof(SEXP), 0, 1);
  (*((SEXP *) Data_custom_val(result))) = sexp;
  return result;
}

#define Sexp_val(sexp) (*((SEXP *) Data_custom_val(sexp)))

#define Val_vecsexp(x) Val_sexp(x)
#define Vecsexp_val(x) ((VECSEXP) Sexp_val(x))
