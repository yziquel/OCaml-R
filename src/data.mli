(** {2 Casting and subtyping to low-level.} *)

type 'a t = private sexp
(**  This is the type of a wrapped R value whose semantics
  *  is the corresponding ['a] type in Objective Caml.
  *)

val cast : sexp -> 'a t
(**  This unsafe function allows us to cast a given [R.sexp]
  *  value to a semantically typed ['a R.t] value. This is
  *  a useful function, as R is a dynamically typed language.
  *)

