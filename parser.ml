open Sexptype

exception Parse_incomplete of string
let _ = Callback.register_exception "Parse_incomplete" (Parse_incomplete "any string")
exception Parse_error of string
let _ = Callback.register_exception "Parse_error" (Parse_error "any string")

external parse_sexp : string -> sexp = "parse_sexp"

