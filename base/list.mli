module List : sig

  class type t = object
    inherit S3.t
    method names : string list
  end

  class from_R : sexp -> t
  val t_from_R : sexp -> t

end

