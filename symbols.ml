open Sexptype
open Data

type 'a symbol = string

(* I am not satisfied with the internals of r_sexp_of_symbol. *)
(*external symbol : 'a symbol -> 'a t = "r_sexp_of_symbol"*)

(* There's a lot of stuff concerning symbols and environments in the
   envir.c file of the R source code.
   We will not wrap up findFun, because it essentially is findVar with
   some dynamic type checking for function SEXPs. *)
module Raw6 = struct
  external install : 'a symbol -> 'a sym sxp = "r_install"
end include Raw6
external findvar : 'a symbol t -> 'a promise = "r_findvar"
external findfun : ('a -> 'b) symbol t -> ('a -> 'b) t = "r_findfun"
let symbol : 'a symbol -> 'a promise = fun s -> findvar (install s)

