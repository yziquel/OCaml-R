(** Testing OCaml-R Camlp4 extension. *)


letR t = "10" ;;
let s =
  letR u = "(1..10)" in
R.r_print_value (R.to_int t);;

letR x = "\"coucou\"" in
R.r_print_value (R.to_string x);;
(*let s = <:r<x=(1..$t$)>>;;*)

let f () = foo <-- x; ();;
y <-- x;;
let sexp foo = "" ;;
(*t <-- sexp;;*) 