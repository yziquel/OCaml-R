module Date = struct

  class type t = object
    inherit S3.t
    method as_float : float
    method as_date : CalendarLib.Calendar.Date.t
  end

  class from_R r : t = object (self)
    inherit S3.from_R r
    method as_float = float_of_t underlying
    method as_date = Calendar.Date.from_unixfloat (86400. *. self#as_float)
  end

  let t_from_R r : t = new from_R r

end

