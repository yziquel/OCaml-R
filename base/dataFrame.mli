module DataFrame :
  sig
    class type t =
      object
        method attribute : string -> sexp
        method classes : string list
        method column : int -> sexp
        method element : int -> int -> sexp
        method names : string list
        method row_names : string list
        method underlying : sexp
      end
    val t_from_R : sexp -> t
  end

