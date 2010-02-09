(**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)

external s3_class : sexp -> sexp = "ocamlr_s3_class"

external aux_get_attrib : sexp -> sexp -> sexp = "ocamlr_get_attrib"
let get_attrib s name = aux_get_attrib s (install name)

external get_attributes : sexp -> sexp = "ocamlr_get_attributes"

class type ['a] s3 = object
  val underlying    : 'a t
  method attribute  : 'b. string -> 'b t
  method attributes : (Specification.symbol * sexp) list
  method classes    : string list
end

class ['a] s3_from_R r : ['a] s3 = object
  val underlying = r
  method attribute s = get_attrib underlying s
  method attributes = List.map
    begin function (a, x) -> (Specification.of_symbol a), x end
    (list_of_lisplist (get_attributes underlying))
  method classes = strings_of_t (get_attrib underlying "class")
end

