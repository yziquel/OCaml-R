module DataFrame = struct

  class type t = object
    inherit List.t
    method row_names : string list
  end

  class from_R r : t = object
    inherit List.from_R r
    method row_names = strings_of_t (get_attrib underlying "row.names")
  end

  let t_from_R r : t = new from_R r

end
