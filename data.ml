type 'a t = sexp

let cast : sexp -> 'a t = Obj.magic

type 'a promise = 'a Lazy.t t
