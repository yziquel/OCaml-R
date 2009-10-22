(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2009 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Interface to R. *)

(** Intialises the R interpreter. This fuction is a simple wrapper around the
  * Rf_initEmbeddedR function of the R API.
  *
  * The appropriate environment variables should be set properly before calling
  * function.
  *
  * The first parameter is an array of command-line-like arguments passed to R.
  * @return 1 if successfully initialised. *)
external init_r : string array -> int = "init_r"
val init : ?argv:string array -> unit -> int
external terminate : unit -> unit = "end_r"

module type Rinterface =
  sig
    type sexp
    type symbol = string
    type arg = [ `Anon of sexp | `Named of symbol * sexp ]
    val sexp : string -> sexp
    val sexp_of_symbol : symbol -> sexp
    val set_var : symbol -> sexp -> unit
    val r_print_value : sexp -> unit
    val exec : string -> arg array -> unit
    val to_bool : sexp -> bool
    val to_int : sexp -> int
    val to_float : sexp -> float
    val to_string : sexp -> string
    val of_bool : bool -> sexp
    val of_int : int -> sexp
    val of_float : float -> sexp
    val of_string : string -> sexp
    val to_bool_array : sexp -> bool array
    val to_int_array : sexp -> int array
    val to_float_array : sexp -> float array
    val to_string_array : sexp -> string array
    val of_bool_array : bool array -> sexp
    val of_int_array : int array -> sexp
    val of_float_array : float array -> sexp
    val of_string_array : string array -> sexp
    val get_attrib : sexp -> string -> sexp
    val dim : sexp -> int array
    val dimnames : sexp -> string array
  end

include Rinterface;;

module type R_Environment = sig val variables : (string * string) list end
module Interpreter : functor (Env : R_Environment) -> Rinterface
