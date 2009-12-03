open Sexptype
open Data

type 'a symbol = string

(* There's a lot of stuff concerning symbols and environments in the
   envir.c file of the R source code.
   We will not wrap up findFun, because it essentially is findVar with
   some dynamic type checking for function SEXPs. *)

external install : 'a symbol -> 'a sym sxp = "r_install"

external findvar : 'a symbol t -> 'a promise = "r_findvar"

external findfun : ('a -> 'b) symbol t -> ('a -> 'b) t = "r_findfun"

let symbol : 'a symbol -> 'a promise = fun s -> findvar (install s)

