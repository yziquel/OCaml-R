type 'a t = sexp
type 'a tt = 'a t  (* This is just aliasing to avoid name clashes. *)
type 'a promise = 'a Lazy.t t
