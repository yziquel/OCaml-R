open Sexptype

type 'a t = sexp
type 'a promise = 'a Lazy.t t
