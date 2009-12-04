(* Memory allocation is done via functions exported from memory.c. *)


(**  Allocates a pairlist.
 
  r_alloc_list takes an integer i as argument, and returns an R
  pairlist, also called a LISTSXP, of size i.

  Such a function is necessary in order to avoid the tail
  recursion issue. While doing 'let x = f () in x::(aux y)' is
  an acceptable way to make aux tail recurse in Objective Caml,
  this relies on the way Objective Caml constructs list with
  the :: constructor. This feature is not available when we use
  stub code around the CONS macro for R. Allocating a list and
  filling it in imperatively is a workaround. *)

external alloc_list : int -> 'a lisplist sxp = "r_alloc_list"
