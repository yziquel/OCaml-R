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

module type Environment = sig val env : (string * string) list end

(** Module containing standard environment variables for R. *)

module Standard : Environment

(** Interface to R. *)

exception Initialisation_failed;;

(** Intialize the R interpreter.
   @param env can be used to specify alternative environement variables. Default
    environement is the one defined by the detected R at compile time.
   @param argv can be used to pass command line arguments to the R
    initialization function in C.
   @raise Initialisation_failed if an error occurs.
 *)
val init : ?env:(string * string) list -> ?argv:string array -> unit -> unit

(** Call this function to terminate properly the interpreter. *)
val terminate : unit -> unit

module type Interface =
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

include Interface;;

(** {2 Functor interface}

A functor interface is provided so it is possible to ensure that the interpreter
   is initialized before using the interface. The creation of the interface
   module using the {!Interpreter} functor performs the initialization using
   {!init}. As environement, you can specify your own or use the {!Rstdenv} module.
*)

module Interpreter : functor (Env : Environment) -> Interface
