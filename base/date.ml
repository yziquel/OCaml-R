module Date = struct

  class type t = object
    inherit S3.t
    method as_float : float
  end

  class from_R r : t = object
    inherit S3.from_R r
    method as_float = float_of_t underlying
  end

  let t_from_R r : t = new from_R r

end

