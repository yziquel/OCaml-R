module DataFrame = struct

  class type t = object
    inherit S3.t
    method names : string list
    method row_names : string list
  end

  class from_R r : t = object
    inherit S3.from_R r
    method names     = strings_of_t (get_attrib underlying "names")
    method row_names = strings_of_t (get_attrib underlying "row.names")
  end

  let t_from_R r : t = new from_R r

end
