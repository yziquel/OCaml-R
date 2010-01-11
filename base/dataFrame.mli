module DataFrame :
  sig
    class type t =
      object
        inherit List.t
        method column : int -> sexp
        method element : int -> int -> sexp
        method row_names : string list
      end
    val t_from_R : sexp -> t
  end

