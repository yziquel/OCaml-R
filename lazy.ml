external force_promsxp : prom sxp -> sexp = "ocamlr_eval_sxp"

let force : 'a promise -> 'a t = force_promsxp

(* For lazy evaluation, we have an issue here: R promises
   are recursively forced by eval. This means that the
   OCaml type system would be broken, because we would need
   to have 'a Lazy.t R.t = 'a Lazy.t Lazy.t R.t. There's two
   solutions:

   -1- 'a R.t would denote a R value of type 'a, lazy or not.
       This is suboptimal, because the OCaml type system could
       and should express these lazy semantics.

   -2- Make a dynamic check on the nature of the argument of
       the force function. If it is a lazy lazy value, we
       should force it manually, with OCaml semantics. If not,
       we can run eval on it. *)

