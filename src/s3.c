/**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *
  *  This calls getAttrib, which is part of the API, to access the
  *  "class" attribute. Usage is (weakly) documented in section
  *  5.9.4 "Attributes" of the "Writing R extensions" documentation,
  *  a.k.a. r-exts.pdf.
  *
  *  Note: Calls to getAttrib do a lot of unnecessary dynamic type
  *  checking. This will have to clarified and cleaned up in future
  *  versions of OCaml-R, possibly bypassing the API.
  */

CAMLprim value ocamlr_s3_class (value sexp) {
  return(Val_sexp(getAttrib(Sexp_val(sexp), R_ClassSymbol)));
}


/**  Get an attribute of a given SEXP.
  *
  *  r_get_attrib takes a SEXP as first argument, then an R symbol
  *  name, and returns the attribute of the first argument matching
  *  this symbol name. Such symbols can be created with the install
  *  function, from Objective Caml side.
  *
  *  Note: As above, lots a dynamic type checking that will have to
  *  be bypassed.
  */

CAMLprim value ocamlr_get_attrib (value sexp, value symbolname) {
  return(Val_sexp(getAttrib(Sexp_val(sexp), Sexp_val(symbolname))));
}


/**  Get the attribute list of a given SEXP.
  *
  *  r_get_attributes takes a SEXP as first argument, and returns
  *  the attributes of the given SEXP.
  */

CAMLprim value ocamlr_get_attributes (value sexp) {
  return(Val_sexp(ATTRIB(Sexp_val(sexp))));
}
