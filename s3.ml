(**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)

external s3_class : sexp -> sexp = "r_s3_class"

external aux_get_attrib : sexp -> sexp -> sexp = "r_get_attrib"

let get_attrib s name = aux_get_attrib s (install name)
