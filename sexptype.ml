(* Argument types for the polymorphic 'a sexp type. *)
type nil                        (* For NILSXP *)
type 'a sym                     (* For SYMSXP *)
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
type vec_int = int vec          (* For INTSXP *)
type vec_str = string vec       (* For STRSXP *)
  (* Or shouldn't it be int32 vec ? *)

(* Types of wrapped R SEXP values. sxp is a polymorphic type
   wrapping up the monomorphic type sexp *)
type sexp
type 'a sxp = sexp
type 'a vecsxp = 'a vec sxp

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

external sexptype_of_sexp : sexp -> int = "r_sexptype_of_sexp"
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

