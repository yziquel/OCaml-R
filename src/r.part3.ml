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
           [init and terminate functions.]

     -2- Static types for R values.
           [types 'a R.t, 'a R.promise, R.Raw.sexp, and phantom sexptypes.]

     -3- R constants - global symbols in libR.so.
           [null_creator.]

     -4- Conversion of R types from OCaml side to R side.
           [sexp_of_t : R.t -> raw sexp.]

     -5- Runtime types internal to R.
           [runtime algebraic types, *NOT* static phantom types.]
           [sexptype : 'a sexp -> sexptype.]

     -6- Conversion of R types from R side to OCaml side.
           [t_of_sexp : sexp -> 'a R.t.]

     -7- Beta-reduction in R.
           [R.eval : sexp list -> sexp]
           [R.force : 'a R.promise -> 'a R.t]
           [R.mlfun : ('a -> 'b) R.t -> 'a R.t -> 'b R.t]

     -8- Dealing with the R symbol table.
           [R.symbol : 'a symbol -> 'a R.t]

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
  type nil                        (* For NILSXP *)
  type sym                        (* For SYMSXP *)
  type 'a lisplist                (* For LISTSXP, and LANGSXP *)
  type simple                     (* For LISTSXP *)
  type pairlist = simple lisplist (* For LISTSXP *)
  type clos                       (* For CLOSXP *)
  type env                        (* For ENVSXP *)
  type prom                       (* For PROMSXP *)
  type call                       (* For LANGSXP *)
  type lang = call lisplist       (* For LANGSXP *)
  type builtin                    (* For BUILTINSXP *)
  type 'a vec                     (* For all the VECSXPs *)
  type vec_char = char vec        (* For CHARSXP *)
  type vec_int = int vec          (* For INTSEXP *)
    (* Or shouldn't it be int32 vec ? *)

  (* Types of wrapped R SEXP values. sxp is a polymorphic type
     wrapping up the monomorphic type sexp *)
  type sexp
  type 'a sxp = sexp
  type 'a vecsxp = 'a vec sxp

  external sexp_equality : sexp -> sexp -> bool = "r_sexp_equality"

end include Raw0

type 'a t = sexp
type 'a promise = 'a Lazy.t t



(* -3- R constants - global symbols in libR.so. *)
(* Commented out until one finds a clean solution
   for the typing of the R NULL. What should it be
   in OCaml? An 'a option mapping to None? *)
external null_creator : unit -> nil sxp = "r_null"



(* -4- Conversion of R types from OCaml side to R side. *)

module Raw1 = struct
  (* sexp will be an opaque type in the interface, and so
     will 'a t. Therefore we provide this conversion function. *)
  let sexp_of_t : 'a t -> sexp = fun x -> x
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

external sexptype_of_sexp : sexp -> int = "r_sexptype_of_sexp"
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

let t_of_sexp : sexp -> 'a t = fun x -> x (* Extremely unsafe... *)



(* -7- Beta-reduction in R. *)

(* -7.1- Execution of calls. *)

external langsxp_of_list : sexp list -> int -> lang sxp = "r_langsxp_of_list"
external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval (l : sexp list) : sexp =
  (* The typing of l needs to be amended and moved to 'a t stuff. *)
  eval_langsxp (langsxp_of_list l (List.length l))

(* -7.2- Forcing R promises. *)

module Raw4 = struct

  external force_promsxp : prom sxp -> sexp = "r_eval_sxp"

end include Raw4

(* It seems to me that the R.force function breaks the type
   system: forcing a 'a Lazy.t promise would not give a 'a promise...
   In R, promises are forced recursively ... *)
let force : 'a promise -> 'a t = force_promsxp

(* -7.3- Mapping functions. *)

let mlfun (f: ('a -> 'b) t) (x: 'a) : 'b t = eval [f; x]


(* -8- Dealing with the R symbol table. *)

type 'a symbol = string

(* I am not satisfied with the internals of r_sexp_of_symbol. *)
external symbol : 'a symbol -> 'a t = "r_sexp_of_symbol"



(* -9- Data conversions. *)

(* -9.1- Conversion of strings. *)

external string_of_charsxp : vec_char sxp -> string = "r_string_of_charsxp"
let string_of_t : string t -> string = string_of_charsxp
external string : string -> string t = "r_charsxp_of_string"

(* -9.2- Conversion of vectors of integers. *)

external access_int_vecsxp : vec_int sxp -> int -> int = "r_access_int_vecsxp"
external length_of_vecsxp : 'a vecsxp -> int = "inspect_vecsxp_length"
let int_list_of_int_vecsxp s =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    (access_int_vecsxp s (lngth - n))::(aux (n - 1) s)
  in aux lngth s




(* Code that is left to audit. *)

(*type arg = [
    `Named of symbol * raw sexp
  | `Anon of raw sexp
  ]*)

module Raw5 = struct

  external eval_string : string -> sexp = "r_sexp_of_string"

end include Raw5

(*external set_var : symbol -> 'a sexp -> unit = "r_set_var"*)
(*external print : 'a sexp -> unit = "r_print_value"*)

(*external exec : string -> arg array -> unit = "r_exec"
  Commented out because of C warning. Will uncoment when OCaml-R compiles on 64 bits. *)

(*external to_bool : 'a sexp -> bool = "bool_of_sexp"*)
(*external to_int : 'a sexp -> int = "int_of_sexp"*)
(*external to_float : 'a sexp -> float = "float_of_sexp"*)
(*external to_string : 'a sexp -> string = "string_of_sexp"*)

(*external of_bool : bool -> 'a sexp = "sexp_of_bool"*)
(*external of_int : int -> 'a sexp = "sexp_of_int"*)
(*external of_float : float -> 'a sexp = "sexp_of_float"*)
(*external of_string : string -> 'a sexp = "sexp_of_string"*)

(*external to_bool_array : 'a sexp -> bool array = "bool_array_of_sexp"
  Commented out while working on 64 bits compilation. *)
(*external to_int_array : 'a sexp -> int array = "int_array_of_sexp"
  Commented out while working on 64 bits compilation. *)
(*external to_float_array : 'a sexp -> float array = "float_array_of_sexp"*)
(*external to_string_array : 'a sexp -> string array = "string_array_of_sexp"
  Commented out while working on 64 bits compilation. *)

(*external of_bool_array : bool array -> 'a sexp = "sexp_of_bool_array"*)
(*external of_int_array : int array -> 'a sexp = "sexp_of_int_array"*)
(*external of_float_array : float array -> 'a sexp = "sexp_of_float_array"*)
(*external of_string_array : string array -> 'a sexp = "sexp_of_string_array"*)

(*external get_attrib : 'a sexp -> string -> 'b sexp = "r_get_attrib"*)

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

(* This functorial loading is bad, because it's not correct
   from a typing point of view. A good solution would be a
   camlp4 extension to write these library description
   automatically, directly from the R library itself. *)
(*module type LibraryDescription = sig
  val name : string
  val symbols : string list
end

module type Library = sig
  val root : sexp list
end*)

module type Interpreter = sig
  (*module Require : functor (L : LibraryDescription) -> Library*)
end

module Interpreter (Env : Environment) : Interpreter = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~sigs: Env.signal_handlers
                ()

  (*module Require (Lib : LibraryDescription) : Library = struct
    let () = ignore (eval_string ("require("^Lib.name^")"))
    let root = List.map symbol Lib.symbols
  end*)

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
  include Raw5

  exception Parse_incomplete of string
  let _ = Callback.register_exception "Parse_incomplete" (Parse_incomplete "any string")
  exception Parse_error of string
  let _ = Callback.register_exception "Parse_error" (Parse_error "any string")

  external parse_sexp : string -> sexp = "parse_sexp"
end

module Internal = struct

  module type Types = sig

    type t
    val recursive : t Lazy.t -> t
    val build : (sexp -> t) -> sexp -> t

  end

  module Parser (M : Types) = struct

    (* General parsing function for internal R structures, i.e. SEXPs. *)

    let t_of_sexp (s : sexp) =
      let rec aux sexps_seen s =
        let is_found (ss, _) = sexp_equality s ss in
        begin match (try Some (List.find is_found sexps_seen) with _ -> None) with
        | None -> let rec x = lazy (M.build (aux ((s, x)::sexps_seen)) s) in Lazy.force x
        | Some (_, t_lazy) -> M.recursive t_lazy
        end
      in aux [] s

  end

  (* Inspection functions. *)

  external inspect_primsxp_offset  : builtin sxp -> int = "inspect_primsxp_offset"

  external inspect_symsxp_pname    : sym sxp -> sexp = "inspect_symsxp_pname"
  external inspect_symsxp_value    : sym sxp -> sexp = "inspect_symsxp_value"
  external inspect_symsxp_internal : sym sxp -> sexp = "inspect_symsxp_internal"

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

  module CTypes = struct

    (* Type definitions. *)

    type t = | Recursive of t Lazy.t | Val of t_val

    and t_val = {
      (* sxpinfo : sxpinfo;   *)
      (* attrib  : t;         *)
      (* gengc_nextnode : t;  *)
      (* gengc_prevnode : t;  *)
      content : t_content
    }

    (* and sxpinfo = {
      type  : sexptype;
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

    let recursive x = Recursive (lazy (Lazy.force x))

    let build rec_build s =
      match sexptype s with
      | NilSxp     -> Val { content = NILSXP }
      | SymSxp     -> Val { content = SYMSXP {
          pname      = rec_build (inspect_symsxp_pname    s);
          sym_value  = rec_build (inspect_symsxp_value    s);
          internal   = rec_build (inspect_symsxp_internal s)}}
      | ListSxp    -> Val { content = LISTSXP {
          carval     = rec_build (inspect_listsxp_carval s);
          cdrval     = rec_build (inspect_listsxp_cdrval s);
          tagval     = rec_build (inspect_listsxp_tagval s)}}
      | ClosSxp    -> Val { content = CLOSXP {
          formals    = rec_build (inspect_closxp_formals s);
          body       = rec_build (inspect_closxp_body    s);
          clos_env   = rec_build (inspect_closxp_env     s)}}
      | EnvSxp     -> Val { content = ENVSXP {
          frame      = rec_build (inspect_envsxp_frame   s);
          enclos     = rec_build (inspect_envsxp_enclos  s);
          hashtab    = rec_build (inspect_envsxp_hashtab s)}}
      | PromSxp    -> Val { content = PROMSXP {
          prom_value = rec_build (inspect_promsxp_value s);
          expr       = rec_build (inspect_promsxp_expr  s);
          prom_env   = rec_build (inspect_promsxp_env   s)}}
      | LangSxp    -> Val { content = LANGSXP {
          carval     = rec_build (inspect_listsxp_carval s);
          cdrval     = rec_build (inspect_listsxp_cdrval s);
          tagval     = rec_build (inspect_listsxp_tagval s)}}
      | SpecialSxp -> Val { content = SPECIALSXP }
      | BuiltinSxp -> Val { content = BUILTINSXP (inspect_primsxp_offset s)}
      | CharSxp    -> Val { content = CHARSXP (string_of_charsxp s) }
      | LglSxp     -> Val { content = LGLSXP }
      | IntSxp     -> Val { content = INTSXP (int_list_of_int_vecsxp s)}
      | RealSxp    -> Val { content = REALSXP }
      | CplxSxp    -> Val { content = CPLXSXP }
      | StrSxp     -> Val { content = STRSXP }
      | DotSxp     -> Val { content = DOTSXP }
      | AnySxp     -> Val { content = ANYSXP }
      | VecSxp     -> Val { content = VECSXP }
      | ExprSxp    -> Val { content = EXPRSXP }
      | BcodeSxp   -> Val { content = BCODESXP }
      | ExtptrSxp  -> Val { content = EXTPTRSXP }
      | WeakrefSxp -> Val { content = WEAKREFSXP }
      | RawSxp     -> Val { content = RAWSXP }
      | S4Sxp      -> Val { content = S4SXP }
      | FunSxp     -> Val { content = FUNSXP }

  end

  module PrettyTypes = struct

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

    let recursive x = Recursive (lazy (Lazy.force x))

    exception Sexp_to_inspect of sexp
    exception Esoteric of sexp

    let symbol_of_symsxp builder (s : sym sxp) =
      let pname    = inspect_symsxp_pname    s
      and value    = inspect_symsxp_value    s
      and internal = inspect_symsxp_internal s in
      match (sexptype pname), (sexptype value), (sexptype internal) with
      | (NilSxp,  _, NilSxp) when sexp_equality s value -> SYMBOL None
      | (CharSxp, BuiltinSxp, NilSxp) | (CharSxp, SpecialSxp, NilSxp) ->
          let symbol_name = string_of_charsxp pname in
          SYMBOL (Some (symbol_name, (builder value)))
      | (CharSxp, SymSxp, NilSxp) ->
          begin match (sexp_equality s value) &&
                      ("" = string_of_charsxp pname) with
          | true -> PLACE | false ->
          begin match (sexp_equality value (inspect_symsxp_value value))  &&
                      (NilSxp = sexptype (inspect_symsxp_pname value))    &&
                      (NilSxp = sexptype (inspect_symsxp_internal value)) with
          | true -> ARG (string_of_charsxp pname)
          | false -> raise (Esoteric s)
          end end
      | _ -> raise (Esoteric s)

    let rec list_of_listsxp builder s =
      let carval = inspect_listsxp_carval s
      and cdrval = inspect_listsxp_cdrval s
      and tagval = inspect_listsxp_tagval s in
      (* Strict parsing of the LIST:
      LIST begin match sexptype tagval with
      | NilSxp ->  (builder carval) :: begin
                   match builder cdrval with
                   | LIST l -> l | NULL -> []
                   | _ -> raise (Esoteric s) end
      | _ -> raise Esoteric end *)
      (* Lax parsing of the LIST: *)
      LIST begin ((builder tagval), (builder carval))::
        begin match builder cdrval with
        | LIST l -> l | NULL -> []
        | _ -> raise (Esoteric s) end
      end

    let rec build rec_build =
      let phi = fun f -> f (build rec_build) in
      function s -> match sexptype s with
      | NilSxp     -> NULL
      | SymSxp     -> begin try phi symbol_of_symsxp (Obj.magic s) with
                      | Esoteric s -> Unknown end
      | ListSxp    -> begin try phi list_of_listsxp s with
                      | Esoteric _ -> Unknown end
      | ClosSxp    -> CLOSURE {
          formals  = rec_build (inspect_closxp_formals s);
          body     = rec_build (inspect_closxp_body    s);
          clos_env = rec_build (inspect_closxp_env     s)}
      | EnvSxp     -> ENV {
          frame   = rec_build (inspect_envsxp_frame   s);
       (* enclos  = rec_build (inspect_envsxp_enclos  s); *) (* We do not care for now. *)
          hashtab = rec_build (inspect_envsxp_hashtab s)}
      | PromSxp    -> PROMISE {
          value    = rec_build (inspect_promsxp_value s);
          expr     = rec_build (inspect_promsxp_expr  s);
          prom_env = rec_build (inspect_promsxp_env   s)}
      | LangSxp    ->
          let carval = inspect_listsxp_carval s
          and cdrval = inspect_listsxp_cdrval s
          and tagval = inspect_listsxp_tagval s in
          begin match build rec_build cdrval with
          | LIST l -> begin match sexptype tagval with
                      | NilSxp -> CALL ((build rec_build carval), l)
                      | _ -> Unknown end
          | _ -> Unknown end
      | SpecialSxp -> Unknown
      | BuiltinSxp -> BUILTIN
      | CharSxp    -> STRING (string_of_charsxp s)
      | LglSxp     -> Unknown
      | IntSxp     -> INT (int_list_of_int_vecsxp s)
      | RealSxp    -> Unknown
      | CplxSxp    -> Unknown
      | StrSxp     -> Unknown
      | DotSxp     -> Unknown
      | AnySxp     -> Unknown
      | VecSxp     -> Unknown
      | ExprSxp    -> Unknown
      | BcodeSxp   -> Unknown
      | ExtptrSxp  -> Unknown
      | WeakrefSxp -> Unknown
      | RawSxp     -> Unknown
      | S4Sxp      -> Unknown
      | FunSxp     -> Unknown

  end

  module CParser = Parser (CTypes)

  module PrettyParser = Parser (PrettyTypes)

  module C = struct
    include CTypes
    include CParser
  end

  module Pretty = struct
    include PrettyTypes
    include PrettyParser
  end

end
