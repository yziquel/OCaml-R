(* This file contains wrappers around allocation functions,
   returning uninitialised pairlists and vectors. *)

external alloc_list        : int -> 'a lisplist sxp = "ocamlr_alloc_list"
external alloc_lgl_vector  : int -> vec_lgl     sxp = "ocamlr_alloc_lgl_vector"
external alloc_int_vector  : int -> vec_int     sxp = "ocamlr_alloc_int_vector"
external alloc_real_vector : int -> vec_real    sxp = "ocamlr_alloc_real_vector"
external alloc_str_vector  : int -> vec_str     sxp = "ocamlr_alloc_str_vector"

