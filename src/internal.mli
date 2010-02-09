(**  {2 Inspection and specification of internals.} *)

(**  Provides a module with strong data types and typing,
  *  aiming to be an indirect specification of low-level
  *  structure of SEXPs.
  *)
module Specification : sig

  (** Semantic description of [SYMSXP] structures. *)
  type symbol = (string * (sexp option)) option option

end

(**  Provides facilities to inspect internal structure of
  *  SEXPs. Useful in the toplevel when you encounter
  *  unexpected R values. *)
module Pretty : sig

  (**  Semantic interpretation and description of SEXPs. *)
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
    | SPECIAL of int
    | BUILTIN
    | STRING of string
    | STRINGS of string list
    | INTS of int list
    | VECSXP of t list
    | BOOLS of bool list
    | FLOATS of float list
    | Unknown

  and closure     = { formals: t; body: t; clos_env: t }
  and environment = { frame: t }
  and promise     = { value: t; expr: t; prom_env: t }

  and pairlist = (t * t) list

  (**  Analyses recursively the structure of a given SEXP. *)
  val t_of_sexp : sexp -> t

end

