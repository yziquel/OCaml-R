module Stub = struct

  let sample = lazy (symbol "sample")

end

(* If we do not expect, it might be better to create an
   R.safe_eval function to evaluate stuff without R-side
   error checking. This might reduce overhead. Will have
   to be implemented in reduction.c. *)
let sample (x : 'a list t) size ?replace ?(prob: float list option) () : 'a list t =
  (* Note that size may be left out in this R function. This
     behaviour does not fit the R.arg behaviour, nor does it
     fit the R.opt behaviour (since the argument should be
     named. This type of argument has to be worked out... *)
  eval (Lazy.force Stub.sample) [
    (arg (fun x -> x)           x)      ;
    (arg int                    size)   ;
    (opt bool         "replace" replace);
    (opt floats       "prob"    prob)   ]

