(* Inspection functions. *)

external inspect_primsxp_offset  : builtin sxp -> int = "inspect_primsxp_offset"

external inspect_symsxp_pname    : 'a sym sxp -> sexp = "inspect_symsxp_pname"
external inspect_symsxp_value    : 'a sym sxp -> sexp = "inspect_symsxp_value"
external inspect_symsxp_internal : 'a sym sxp -> sexp = "inspect_symsxp_internal"

external inspect_listsxp_carval  : 'a lisplist sxp -> sexp = "inspect_listsxp_carval"
external inspect_listsxp_cdrval  : 'a lisplist sxp -> sexp = "inspect_listsxp_cdrval"
external inspect_listsxp_tagval  : 'a lisplist sxp -> sexp = "inspect_listsxp_tagval"

external inspect_envsxp_frame    : env sxp -> sexp = "inspect_envsxp_frame"
external inspect_envsxp_enclos   : env sxp -> sexp = "inspect_envsxp_enclos"
external inspect_envsxp_hashtab  : env sxp -> sexp = "inspect_envsxp_hashtab"

external inspect_closxp_formals  : clos sxp -> sexp = "inspect_closxp_formals"
external inspect_closxp_body     : clos sxp -> sexp = "inspect_closxp_body"
external inspect_closxp_env      : clos sxp -> sexp = "inspect_closxp_env"

external inspect_promsxp_value   : prom sxp -> sexp = "inspect_promsxp_value"
external inspect_promsxp_expr    : prom sxp -> sexp = "inspect_promsxp_expr"
external inspect_promsxp_env     : prom sxp -> sexp = "inspect_promsxp_env"

external length_of_vecsxp : 'a vecsxp -> int = "inspect_vecsxp_length"

external access_lgl_vecsxp  : vec_lgl  sxp -> int -> bool   = "r_access_lgl_vecsxp"
external access_int_vecsxp  : vec_int  sxp -> int -> int    = "r_access_int_vecsxp"
external access_real_vecsxp : vec_real sxp -> int -> float  = "r_access_real_vecsxp"
external access_str_vecsxp  : vec_str  sxp -> int -> string = "r_access_str_vecsxp"
external access_sexp_vecsxp : vec_sexp sxp -> int -> sexp   = "r_access_sexp_vecsxp"
