(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2010 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*             guillaume.yziquel@citycable.ch                                    *)
(*********************************************************************************)

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

