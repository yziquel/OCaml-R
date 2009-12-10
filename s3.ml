(**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)

external s3_class : sexp -> sexp = "r_s3_class"

external aux_get_attrib : sexp -> sexp -> sexp = "r_get_attrib"

let get_attrib s name = aux_get_attrib s (install name)

module S3 = struct

  class type t = object

    val underlying : 'a tt

  end

  class from_R r : t = object

    val underlying = r

  end

  let t_from_R r : t = new from_R r

end

