open Sexptype
open Sexprec
open Symbols
open Conversion
open Read_internal

(* This module is an ugly work in progress. It is supposed to be ugly...
   The purpose of this module is to investigate how to make proper function
   calls via the eval function of eval.c. *)
 
external apply_closure : lang sxp -> clos sxp -> pairlist sxp -> sexp = "r_apply_closure"
