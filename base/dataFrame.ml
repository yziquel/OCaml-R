module DataFrame = struct

  let subset2 = findfun (install "[[.data.frame")

  class type t = object
    inherit List.t
    method row_names : string list
    method column : int -> sexp
    method element : int -> int -> sexp
  end

  class from_R r : t = object
    inherit List.from_R r
    method row_names = strings_of_t (get_attrib underlying "row.names")
    method column x = eval_langsxp (langsxp subset2 [None, (int x)])
    method element x y = eval_langsxp (langsxp subset2 [None, (int x); None, (int y)])
  end

  let t_from_R r : t = new from_R r

end
