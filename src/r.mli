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
type 'a t
type 'a promise = 'a Lazy.t t

(* R constants - global symbols in libR.so. *)
(* such as NULL... *)

(** Beta-reduction in R. *)
module Raw0 : sig
  type sexp
end
type sexp = Raw0.sexp

val eval : sexp list -> sexp
val force : 'a promise -> 'a t

val mlfun : ('a -> 'b) t -> 'a t -> 'b t

(** Dealing with the R symbol table. *)
type 'a symbol = string
val symbol : 'a symbol -> 'a t



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
  val root : sexp list
end

module type Interpreter = sig
  module Require : functor (L : LibraryDescription) -> Library
end

module Interpreter : functor (Env : Environment) -> Interpreter


(* The Raw module is here to help people deal with internals of the R
   module. It will eventually be hidden. *)

module Raw : sig

  type sexp = Raw0.sexp

  val sexp_of_t : 'a t -> sexp
  val sexp_equality : sexp -> sexp -> bool

  type sexptype =
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

  val sexptype : sexp -> sexptype

end

module Internal : sig

  module C : sig

    type t = | Recursive of t Lazy.t | Val of t_val

    and t_val = {
      (* sxpinfo : sxpinfo; *)
      (* attrib  : t; *)
      (* gengc_nextnode : t; *)
      (* gengc_prevnode : t; *)
      content : t_content
    }

    and t_content =
      | NILSXP
      | SYMSXP of sxp_sym
      | LISTSXP of sxp_list
      | CLOSXP of sxp_clos
      | ENVSXP of sxp_env
      | PROMSXP of sxp_prom
      | LANGSXP of sxp_list
      | SPECIALSXP
      | BUILTINSXP of int
      | CHARSXP of string
      | LGLSXP
      | INTSXP of int list
      | REALSXP
      | CPLXSXP
      | STRSXP
      | DOTSXP
      | ANYSXP
      | VECSXP
      | EXPRSXP
      | BCODESXP
      | EXTPTRSXP
      | WEAKREFSXP
      | RAWSXP
      | S4SXP
      | FUNSXP

    and sxp_sym  = { pname: t; sym_value: t; internal: t }
    and sxp_list = { carval: t; cdrval: t; tagval: t }
    and sxp_env  = { frame: t; enclos: t; hashtab: t }
    and sxp_clos = { formals: t; body: t; clos_env: t }
    and sxp_prom = { prom_value: t; expr: t; prom_env: t }

    val t_of_sexp : Raw.sexp -> t

  end

  module Pretty : sig

    type t =
      | Recursive of t Lazy.t
      | NULL
      | SYMBOL of (string * t) option
      | ARG of string
      | PLACE
      | LIST of pairlist
      | CLOSURE of closure
      | ENV of environment
      | PROMISE of promise
      | CALL of t * pairlist
      | BUILTIN
      | STRING of string
      | INT of int list
      | Unknown

    and closure     = { formals: t; body: t; clos_env: t }
    and environment = { frame: t; (* enclos: t; *) hashtab: t }
    and promise     = { value: t; expr: t; prom_env: t }

    and pairlist = (t * t) list (* For strict list parsing, t list. *)

    exception Sexp_to_inspect of Raw.sexp

    val t_of_sexp : Raw.sexp -> t

  end

end
