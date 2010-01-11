module List : sig

  class type t = object

    inherit S3.t
    method names : string list

  end

  val t_from_R : sexp -> t

end