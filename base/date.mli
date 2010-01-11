module Date : sig

  class type t = object
    inherit S3.t
    method as_float : float
  end

  val t_from_R : sexp -> t

end

