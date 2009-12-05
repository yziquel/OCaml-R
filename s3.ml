(**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)

external s3_class : sexp -> string = "r_s3_class"
