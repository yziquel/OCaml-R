(* Low-level data manipulation functions. *)

(* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. *)

external inspect_attributes : sexp      -> sexp = "ocamlr_inspect_attributes"
external length_of_vecsxp   : 'a vecsxp -> int  = "ocamlr_inspect_vecsxp_length"

external inspect_primsxp_offset  : builtin sxp     -> int  = "ocamlr_inspect_primsxp_offset"
external inspect_symsxp_pname    : sym sxp         -> sexp = "ocamlr_inspect_symsxp_pname"
external inspect_symsxp_value    : sym sxp         -> sexp = "ocamlr_inspect_symsxp_value"
external inspect_symsxp_internal : sym sxp         -> sexp = "ocamlr_inspect_symsxp_internal"
external inspect_listsxp_carval  : 'a lisplist sxp -> sexp = "ocamlr_inspect_listsxp_carval"
external inspect_listsxp_cdrval  : 'a lisplist sxp -> sexp = "ocamlr_inspect_listsxp_cdrval"
external inspect_listsxp_tagval  : 'a lisplist sxp -> sexp = "ocamlr_inspect_listsxp_tagval"
external inspect_envsxp_frame    : env sxp         -> sexp = "ocamlr_inspect_envsxp_frame"
external inspect_envsxp_enclos   : env sxp         -> sexp = "ocamlr_inspect_envsxp_enclos"
external inspect_envsxp_hashtab  : env sxp         -> sexp = "ocamlr_inspect_envsxp_hashtab"
external inspect_closxp_formals  : clos sxp        -> sexp = "ocamlr_inspect_closxp_formals"
external inspect_closxp_body     : clos sxp        -> sexp = "ocamlr_inspect_closxp_body"
external inspect_closxp_env      : clos sxp        -> sexp = "ocamlr_inspect_closxp_env"
external inspect_promsxp_value   : prom sxp        -> sexp = "ocamlr_inspect_promsxp_value"
external inspect_promsxp_expr    : prom sxp        -> sexp = "ocamlr_inspect_promsxp_expr"
external inspect_promsxp_env     : prom sxp        -> sexp = "ocamlr_inspect_promsxp_env"

external access_lgl_vecsxp  : vec_lgl  sxp -> int -> bool     = "ocamlr_access_lgl_vecsxp"
external access_int_vecsxp  : vec_int  sxp -> int -> int      = "ocamlr_access_int_vecsxp"
external access_real_vecsxp : vec_real sxp -> int -> float    = "ocamlr_access_real_vecsxp"
external access_str_vecsxp  : vec_str  sxp -> int -> string   = "ocamlr_access_str_vecsxp"
external access_sexp_vecsxp : vec_sexp sxp -> int -> sexp     = "ocamlr_access_sexp_vecsxp"
external access_expr_vecsxp : vec_expr sxp -> int -> lang sxp = "ocamlr_access_sexp_vecsxp"

