(** {2 Low-level SEXP typing.} *)

type sexp
(**  A universal type for wrapped R values. *)

type 'a sxp
(**  A polymorphic type for wrapped R values. ['a] represents
  *  the dynamic typing of the underlying R value. *)

type env
(**  Phantom type representing the ENVSXP dynamic R sexptype. *)

type lang
(**  Phantom type representing the LANGSXP dynamic R sexptype. *)

type 'a vec
(**  Polymorphic phantom type representing R array sexptypes. *)

type 'a vecsxp = 'a vec sxp
(**  Type abreviation for R array values. *)

type sexptype =
(**  Algebraic datatype reflecting R's dynamic typing. *)
  | NilSxp
  | SymSxp
  | ListSxp
  | CloSxp
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
(**  Returns the R dynamic typing of a wrapped R value. *)

