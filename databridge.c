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


CAMLprim value Val_sexp (SEXP sexp) {
  value result = caml_alloc_small(1, Abstract_tag);
  Field(result, 0) = (value) sexp;
    /* Do not use Val_long in the above statement,
       as it will drop the top bit. See mlvalues.h. */
  return result;
}

#define Sexp_val(sexp) ((SEXP) Field(sexp, 0)

#define Val_vecsexp(x) Val_sexp(x)
#define Vecsexp_val(x) ((VECSEXP) Sexp_val(x))