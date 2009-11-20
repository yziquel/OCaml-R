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

(* This is the end of the Standard module, constructed via r.ml.part1 and .part2. *)
end


(* Summary:

     -1- Functions to initialise and terminate the R interpreter.
           [init_r and terminate functions.]

     -2- Static types for R values.
           [types R.t and 'a R.Raw.sexp, and phantom sexptypes.]

     -3- R constants - global symbols in libR.so.
           [null/null_creator.]

     -4- Conversion of R types from OCaml side to R side.
           [sexp_of_t : R.t -> raw sexp.]

     -5- Runtime types internal to R.
           [runtime algebraic types, *NOT* static phantom types.]
           [sexptype : 'a sexp -> sexptype.]

     -6- Conversion of R types from R side to OCaml side.
           [t_of_sexp : 'a sexp -> R.t.]

     -7- Beta-reduction in R.
           [R.eval : R.t list -> R.t.]

     -8- Dealing with the R symbol table.
           [R.symbol : symbol -> R.t]

================================================================================ *)

(* -1- Functions to initialise and terminate the R interpreter. *)

external init_r : string array -> int -> int = "init_r"
external terminate : unit -> unit = "end_r"

exception Initialisation_failed

let init ?(name    = try Sys.argv.(0) with _ -> "OCaml-R")
         ?(argv    = try List.tl (Array.to_list Sys.argv) with _ -> [])
         ?(env     = Standard.env)
         ?(sigs    = Standard.signal_handlers) () =
  List.iter (function name, value -> Unix.putenv name value) env;
  let r_sigs = match sigs with true -> 0 | false -> 1 in
  match init_r (Array.of_list (name::argv)) r_sigs with
  | 1 -> () | _ -> raise Initialisation_failed


(* -2- Static types for R values. *)

module Raw0 = struct

  (* Argument types for the polymorphic 'a sexp type. *)
  type nil          (* For NILSXP *)
  type lang         (* For LANGSXP *)
  type char         (* For CHARSEXP *)
  type raw          (* universal type 'raw sexp' *)

  (* Types of wrapped R SEXP values. sexp is a polymorphic type. *)
  type 'a sexp

  (* Forgetful 'functor'. *)
  let forget_sexptype : 'a sexp -> raw sexp = Obj.magic

  external sexp_equality : 'a sexp -> 'b sexp -> bool = "r_sexp_equality"

end include Raw0

type t = Sexp of raw sexp | NULL



(* -3- R constants - global symbols in libR.so. *)

external null_creator : unit -> nil sexp = "r_null"
let null = NULL



(* -4- Conversion of R types from OCaml side to R side. *)

module Raw1 = struct
  let sexp_of_t : t -> raw sexp = function
    | Sexp s -> s
    | NULL -> forget_sexptype (null_creator ())
end include Raw1



(* -5- Runtime types internal to R. *)

module Raw2 = struct
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
end include Raw2

external sexptype_of_sexp : 'a sexp -> int = "r_sexptype_of_sexp"
module Raw3 = struct
  let sexptype s = match (sexptype_of_sexp s) with
    | 0  -> NilSxp
    | 1  -> SymSxp
    | 2  -> ListSxp
    | 3  -> ClosSxp
    | 4  -> EnvSxp
    | 5  -> PromSxp
    | 6  -> LangSxp
    | 7  -> SpecialSxp
    | 8  -> BuiltinSxp
    | 9  -> CharSxp
    | 10 -> LglSxp
    (* Integer range is not defined here. *)
    | 13 -> IntSxp
    | 14 -> RealSxp
    | 15 -> CplxSxp
    | 16 -> StrSxp
    | 17 -> DotSxp
    | 18 -> AnySxp
    | 19 -> VecSxp
    | 20 -> ExprSxp
    | 21 -> BcodeSxp
    | 22 -> ExtptrSxp
    | 23 -> WeakrefSxp
    | 24 -> RawSxp
    | 25 -> S4Sxp
    (* 99 represents a 'dummy' type for functions, with is an
       umbrella for Closure, Builtin or Special types. *)
    | 99 -> FunSxp
    | _ -> failwith "R value with type not specified in Rinternals.h"
end include Raw3



(* -6- Conversion of R types from R side to OCaml side. *)

let t_of_sexp s = match sexptype s with
  | NilSxp -> NULL
  | _ -> Sexp s



(* -7- Beta-reduction in R. *)

external langsxp_of_list : 'a sexp list -> int -> lang sexp = "r_langsxp_of_list"
external eval_langsxp : lang sexp -> raw sexp = "r_eval_langsxp"

let eval l =
  let sexps, n = (List.map sexp_of_t l), (List.length l) in
  let langsxp = langsxp_of_list sexps n in
  t_of_sexp (eval_langsxp langsxp)



(* -8- Dealing with the R symbol table. *)

type symbol = string

module Raw4 = struct
  (* Currently, sexp_of_symbol segfaults. This has to be corrected. *)
  external sexp_of_symbol : symbol -> raw sexp = "r_sexp_of_symbol"
end include Raw4

let symbol (sym : symbol) = t_of_sexp (sexp_of_symbol sym)



(* -9- Data conversions. *)

exception Incompatible_sexptype

external string_of_charsexp : char sexp -> string = "r_string_of_charsexp"
(* let string_of_t (t : string R.t) = *)



type arg = [
    `Named of symbol * raw sexp
  | `Anon of raw sexp
  ]

external sexp : string -> raw sexp = "r_sexp_of_string"
external set_var : symbol -> 'a sexp -> unit = "r_set_var"
external print : 'a sexp -> unit = "r_print_value"

(*external exec : string -> arg array -> unit = "r_exec"
  Commented out because of C warning. Will uncoment when OCaml-R compiles on 64 bits. *)

external to_bool : 'a sexp -> bool = "bool_of_sexp"
external to_int : 'a sexp -> int = "int_of_sexp"
external to_float : 'a sexp -> float = "float_of_sexp"
external to_string : 'a sexp -> string = "string_of_sexp"

external of_bool : bool -> 'a sexp = "sexp_of_bool"
external of_int : int -> 'a sexp = "sexp_of_int"
external of_float : float -> 'a sexp = "sexp_of_float"
external of_string : string -> 'a sexp = "sexp_of_string"

(*external to_bool_array : 'a sexp -> bool array = "bool_array_of_sexp"
  Commented out while working on 64 bits compilation. *)
(*external to_int_array : 'a sexp -> int array = "int_array_of_sexp"
  Commented out while working on 64 bits compilation. *)
external to_float_array : 'a sexp -> float array = "float_array_of_sexp"
(*external to_string_array : 'a sexp -> string array = "string_array_of_sexp"
  Commented out while working on 64 bits compilation. *)

external of_bool_array : bool array -> 'a sexp = "sexp_of_bool_array"
external of_int_array : int array -> 'a sexp = "sexp_of_int_array"
external of_float_array : float array -> 'a sexp = "sexp_of_float_array"
external of_string_array : string array -> 'a sexp = "sexp_of_string_array"

external get_attrib : 'a sexp -> string -> 'b sexp = "r_get_attrib"

(*let dim sexp = to_int_array (get_attrib sexp "dim");;
  Commented out while working on 64 bits compilation. *)
(*let dimnames sexp = to_string_array (get_attrib sexp "dimnames");;
  Commented out while working on 64 bits compilation. *)

(*
external to_matrix : (sexp -> 'a) -> sexp -> 'a array array = "to_matrix"
external of_matrix : ('a -> sexp) -> 'a array array -> sexp = "of_matrix"


let to_bool_matrix = to_matrix to_bool;;
let to_int_matrix = to_matrix to_int;;
let to_float_matrix = to_matrix to_float;;
let to_string_matrix = to_matrix to_string;;

let of_bool_matrix = of_matrix of_bool;;
let of_int_matrix = of_matrix of_int;;
let of_float_matrix = of_matrix of_float;;
let of_string_matrix = of_matrix of_string;;
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

module Interpreter (Env : Environment) : Interpreter = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~sigs: Env.signal_handlers
                ()

  module Require (Lib : LibraryDescription) : Library = struct
    let () = ignore (sexp ("require("^Lib.name^")"))
    let root = List.map symbol Lib.symbols
  end

end


(* Raw functions module. The purpose of this module is the help
   people dealing with the internals of the R module. It should
   not be used in the scope of vanilla R / OCaml code. *)

module Raw = struct
  include Raw0
  include Raw1
  include Raw2
  include Raw3
  include Raw4
end

module Internal = struct

  (* Type definitions. *)

  type t = eager_t Lazy.t

  and eager_t = {
    (* sxpinfo : sxpinfo;   *)
    (* attrib  : t;         *)
    (* gengc_nextnode : t; *)
    (* gengc_prevnode : t; *)
    content : t_content
  }

  (* and sxpinfo = {
    type  : Raw.sexptype;
    obj   : int;
    named : int;
    gp    : int;
    mark  : int;
    debug : int;
    trace : int;
    spare : int;
    gcgen : int;
    gccls : int;
  }*)

  and t_content =
    | NILSXP
    | SYMSXP of sxp_sym
    | LISTSXP of sxp_list
    | CLOSSXP
    | ENVSXP
    | PROMSXP of sxp_prom
    | LANGSXP of sxp_list
    | SPECIALSXP
    | BUILTINSXP
    | CHARSXP of string
    | LGLSXP
    | INTSXP
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
  and sxp_list = { carval: t; cdrval: t; tagval: t}
  and sxp_prom = { prom_value: t; expr: t; env: t }

  (* Conversion functions. *)

  external inspect_symsxp_pname    : 'a sexp -> 'b sexp = "inspect_symsxp_pname"
  external inspect_symsxp_value    : 'a sexp -> 'b sexp = "inspect_symsxp_value"
  external inspect_symsxp_internal : 'a sexp -> 'b sexp = "inspect_symsxp_internal"

  external inspect_listsxp_carval  : 'a sexp -> 'b sexp = "inspect_listsxp_carval"
  external inspect_listsxp_cdrval  : 'a sexp -> 'b sexp = "inspect_listsxp_cdrval"
  external inspect_listsxp_tagval  : 'a sexp -> 'b sexp = "inspect_listsxp_tagval"

  external inspect_promsxp_value   : 'a sexp -> 'b sexp = "inspect_promsxp_value"
  external inspect_promsxp_expr    : 'a sexp -> 'b sexp = "inspect_promsxp_expr"
  external inspect_promsxp_env     : 'a sexp -> 'b sexp = "inspect_promsxp_env"

  let rec t_of_sexp ?unfold:(unfold=true) s =
    let rec aux sexps_seen s =
<<<<<<< HEAD
=======
      print_endline (string_of_int (List.length sexps_seen));
>>>>>>> R.Internal.t_of_sexp now works.
      let is_found (ss, _) = sexp_equality s ss in
      match (try Some (List.find is_found sexps_seen) with _ -> None) with
      | Some (_, t) -> lazy (Lazy.force t)
      | None -> let veil x = aux ((s, x)::sexps_seen) in let rec t = lazy
          { content = match sexptype s with
            | NilSxp     -> NILSXP
            | SymSxp     -> SYMSXP {
                pname      = veil t (inspect_symsxp_pname    s);
                sym_value  = veil t (inspect_symsxp_value    s);
                internal   = veil t (inspect_symsxp_internal s)}
            | ListSxp    -> LISTSXP {
                carval     = veil t (inspect_listsxp_carval s);
                cdrval     = veil t (inspect_listsxp_cdrval s);
                tagval     = veil t (inspect_listsxp_tagval s)}
            | ClosSxp    -> CLOSSXP
            | EnvSxp     -> ENVSXP
            | PromSxp    -> PROMSXP {
                prom_value = veil t (inspect_promsxp_value s);
                expr       = veil t (inspect_promsxp_expr  s);
                env        = veil t (inspect_promsxp_env   s)}
            | LangSxp    -> LANGSXP {
                carval     = veil t (inspect_listsxp_carval s);
                cdrval     = veil t (inspect_listsxp_cdrval s);
                tagval     = veil t (inspect_listsxp_tagval s)}
            | SpecialSxp -> SPECIALSXP
            | BuiltinSxp -> BUILTINSXP
            | CharSxp    -> CHARSXP (string_of_charsexp ((Obj.magic s) : char sexp))
            | LglSxp     -> LGLSXP
            | IntSxp     -> INTSXP
            | RealSxp    -> REALSXP
            | CplxSxp    -> CPLXSXP
            | StrSxp     -> STRSXP
            | DotSxp     -> DOTSXP
            | AnySxp     -> ANYSXP
            | VecSxp     -> VECSXP
            | ExprSxp    -> EXPRSXP
            | BcodeSxp   -> BCODESXP
            | ExtptrSxp  -> EXTPTRSXP
            | WeakrefSxp -> WEAKREFSXP
            | RawSxp     -> RAWSXP
            | S4Sxp      -> S4SXP
            | FunSxp     -> FUNSXP
          } in if unfold then ignore (Lazy.force t); t
    in aux [] s

    let rec unfold level t =
      match level with | 0 -> () | _ -> List.iter (unfold (level - 1))
      begin match (Lazy.force t).content with
      | NILSXP     -> []
      | SYMSXP x   -> [x.pname; x.sym_value; x.internal]
      | LISTSXP x  -> [x.carval; x.cdrval; x.tagval]
      | CLOSSXP    -> []
      | ENVSXP     -> []
      | PROMSXP x  -> [x.prom_value; x.expr; x.env]
      | LANGSXP x  -> [x.carval; x.cdrval; x.tagval]
      | SPECIALSXP -> []
      | BUILTINSXP -> []
      | CHARSXP _  -> []
      | LGLSXP     -> []
      | INTSXP     -> []
      | REALSXP    -> []
      | CPLXSXP    -> []
      | STRSXP     -> []
      | DOTSXP     -> []
      | ANYSXP     -> []
      | VECSXP     -> []
      | EXPRSXP    -> []
      | BCODESXP   -> []
      | EXTPTRSXP  -> []
      | WEAKREFSXP -> []
      | RAWSXP     -> []
      | S4SXP      -> []
      | FUNSXP     -> []
      end

end
