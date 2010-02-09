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

(** {2 Evaluation of R code and calls.} *)

exception Runtime_error of lang sxp * string

val eval_langsxp : lang sxp -> 'a t
(**  [eval_langsxp] takes a R value containing an R executable expression.
  *  Also known as a [LANGSXP]. You get the resulting value back. *)

val eval_string : string -> 'a t
(**  [eval_string] takes a string containing R code, and feeds it to the
  *  R interpreter. You get the resulting value back. The typing of this
  *  function is deliberately unsafe in order to allow the user to type
  *  it precisely. *)

val arg : ('a -> 'b t) -> ?name:string -> 'a -> (string option * sexp) option
(**  Convenience function to wrap up arguments, when mapping R functions
  *  to Objective Caml functions. *)

val opt : ('a -> 'b t) -> string -> 'a option -> (string option * sexp) option
(**  Convenience function to wrap up optional arguments, when mapping R functions
  *  to Objective Caml functions. *)

val eval : sexp -> (string option * sexp) option list -> 'a t
(**  [eval f args] evaluates an the R function [f] with respect to a list of
  *  arguments. Argument [None] is ignored, and [Some (name, sexp)] is the
  *  argument whose optional name is [name] and whose value is [sexp]. The
  *  typing of this function is deliberately unsafe in order to allow the
  *  user to type it precisely. *)

