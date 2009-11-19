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



module type Environment = sig
  val name : string
  val options : string list
  val signal_handlers : bool
  val env : (string * string) list
end

(** Module containing standard environment variables for R. *)

module Standard : Environment



(** Interface to R. *)

exception Initialisation_failed;;

(** Initialize the R interpreter.
   @param env can be used to specify alternative environment variables. Default
    environment is the one defined by the detected R at compile time.
   @param argv can be used to pass command line arguments to the R
    initialization function in C.
   @raise Initialisation_failed if an error occurs.
 *)
val init : ?name:string ->
           ?argv:string list ->
           ?env:(string * string) list ->
           ?sigs:bool -> unit -> unit

(** Call this function to terminate properly the interpreter. *)
val terminate : unit -> unit

(** Static types for R values. *)
type t

(* R constants - global symbols in libR.so. *)
val null : t

(** Beta-reduction in R. *)
val eval : t list -> t

(** Dealing with the R symbol table. *)
type symbol = string
val symbol : symbol -> t



(*type symbol = string*)
(*type arg = [ `Anon of Raw.sexp | `Named of symbol * Raw.sexp ]*)

(*val sexptype : Raw.sexp -> Raw.sexptype*)

(*val sexp : string -> Raw.sexp*)
(*val sexp_of_symbol : symbol -> Raw.sexp*)
(*val set_var : symbol -> Raw.sexp -> unit*)
(*val print : Raw.sexp -> unit*)
(*val eval : Raw.sexp list -> Raw.sexp*)
(*val exec : string -> arg array -> unit
  Commented out while working on compilation of 64 bits. *)
(*val to_bool : Raw.sexp -> bool*)
(*val to_int : Raw.sexp -> int*)
(*val to_float : Raw.sexp -> float*)
(*val to_string : Raw.sexp -> string*)
(*val of_bool : bool -> Raw.sexp*)
(*val of_int : int -> Raw.sexp*)
(*val of_float : float -> Raw.sexp*)
(*val of_string : string -> Raw.sexp*)
(*val to_bool_array : Raw.sexp -> bool array
  Commented out while working on 64 bits compilation. *)
(*val to_int_array : Raw.sexp -> int array
  Commented out while working on 64 bits compilation. *)
(*val to_float_array : Raw.sexp -> float array*)
(*val to_string_array : Raw.sexp -> string array
  Commented out while working on 64 bits compilation. *)
(*val of_bool_array : bool array -> Raw.sexp*)
(*val of_int_array : int array -> Raw.sexp*)
(*val of_float_array : float array -> Raw.sexp*)
(*val of_string_array : string array -> Raw.sexp*)
(*val get_attrib : Raw.sexp -> string -> Raw.sexp*)
(*val dim : Raw.sexp -> int array
  Commented out while working on 64 bits compilation. *)
(*val dimnames : Raw.sexp -> string array
  Commented out while working on 64 bits compilation. *)

(** {2 Functor interface}

A functor interface is provided so it is possible to ensure that the interpreter
   is initialized before using the interface. The creation of the interface
   module using the {!Interpreter} functor performs the initialization using
   {!init}. As environement, you can specify your own or use the {!Standard} module.
*)

module type LibraryDescription = sig
  val name : string
  val symbols : string list
end

module type Library = sig
  val root : t list
end

module type Interpreter = sig
  module Require : functor (L : LibraryDescription) -> Library
end

module Interpreter : functor (Env : Environment) -> Interpreter


(* The Raw module is here to help people deal with internals of the R
   module. It will eventually be hidden. *)

module Raw : sig

  type sexp

  val sexp_of_t : t -> sexp

  type internally =
    | NilSxp
    | SymSxp
    | ListSxp
    | ClosSxp
    | EnvSxp
    | PromSxp
    | LangSxp
    | SpecialSxp
    | BuiltinSxp
    | CharSxp
    | LglSxp
    | IntSxp
    | RealSxp
    | CplxSxp
    | StrSxp
    | DotSxp
    | AnySxp
    | VecSxp
    | ExprSxp
    | BcodeSxp
    | ExtptrSxp
    | WeakrefSxp
    | RawSxp
    | S4Sxp
    | FunSxp

  val sexptype : sexp -> internally

end

module Internal : sig

  type t = {
    (* sxpinfo : sxpinfo; *)
    (* attrib  : t; *)
    (* gengc_nextnode : t; *)
    (* gengc_prevnode : t; *)
    content : t_content
  }

  and t_content =
    | Prom of sxp_prom
    | Unknown

  and sxp_prom = {value: t; expr: t; env: t}

  val t_of_sexp : Raw.sexp -> t

end
