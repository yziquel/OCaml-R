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

CAMLprim value r_s3_class (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  SEXP classname = getAttrib(Sexp_val(sexp), R_ClassSymbol);
  result = caml_copy_string(CHAR(classname));
  CAMLreturn(result);
}
