module S3 : sig

  class type t = object
    method underlying : sexp
    method attribute  : string -> sexp
    method attributes : (Specification.symbol * sexp) list
    method classes    : string list
  end

  val t_from_R : sexp -> t

end

