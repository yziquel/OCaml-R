module Base = struct
module List = struct

  class type t = object
    inherit S3.t
    method names : string list
  end

  class from_R r : t = object
    inherit S3.from_R r
    method names     = strings_of_t (get_attrib underlying "names")
  end

  let t_from_R r : t = new from_R r

end
module DataFrame = struct

  let subset2 = lazy (findfun (install "[[.data.frame"))

  class type t = object
    inherit List.t
    method row_names : string list
    method column : int -> sexp
    method element : int -> int -> sexp
  end

  class from_R r : t = object
    inherit List.from_R r
    method row_names = strings_of_t (get_attrib underlying "row.names")
    method column x = eval_langsxp (langsxp (Lazy.force subset2)
      [None, underlying; None, (int x)])
    method element x y = eval_langsxp (langsxp (Lazy.force subset2)
      [None, underlying; None, (int x); None, (int y)])
  end

  let t_from_R r : t = new from_R r

end
end
