type 'a symbol = string

(* There's a lot of stuff concerning symbols and environments in the
   envir.c file of the R source code. *)

external install : 'a symbol -> 'a sym sxp = "r_install"

external findvar : 'a symbol t -> 'a promise = "r_findvar"

external findfun : ('a -> 'b) symbol t -> ('a -> 'b) t = "r_findfun"

let symbol : 'a symbol -> 'a t = fun s ->
  let var = force (findvar (install s)) in

  (* If we try to retrieve a function, we should use findfun. If we
     use findvar, we indeed get a closure, but a closure for a generic
     function in the sense of R objects:

     CLOSURE {formals = LIST [(ARG "object", PLACE); (ARG "...", PLACE)];

     and you get runtime errors. This is why we have a dynamic type check
     here, and this is why this symbol should be used as little as
     possible at R runtime. *)

  match is_function var with
  | false -> var
  | true -> force (findfun (install s))