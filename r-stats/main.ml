module Stub = struct

  let () = ignore (R.eval_string "require(stats, quietly=TRUE)")

  let cor = R.symbol "cor"

  let lm = R.symbol "lm"

  let stl = R.symbol "stl"

  let fischer_test = R.symbol "fischer.test"

  let poisson_test = R.symbol "poisson.test"

end

let cor x ?y ?use ?cor_method () =
  R.eval Stub.cor [
    R.arg (fun x -> x) x                   ;
    R.opt (fun x -> x) "y" y               ;
    R.opt (fun x -> x) "use" use           ;
    R.opt (fun x -> x) "method" cor_method ]

let lm formula ?data ?subset ?weights ?na_action ?lm_method ?model ?x ?y ?qr ?singular_ok ?contrasts ?offset () =
  R.eval Stub.lm [
    R.arg (fun x -> x)                formula     ;
    R.opt (fun x -> x) "data"         data        ;
    R.opt (fun x -> x) "subset"       subset      ;
    R.opt (fun x -> x) "weights"      weights     ;
    R.opt (fun x -> x) "na.action"    na_action   ;
    R.opt (fun x -> x) "method"       lm_method   ;
    R.opt (fun x -> x) "model"        model       ;
    R.opt (fun x -> x) "x"            x           ;
    R.opt (fun x -> x) "y"            y           ;
    R.opt (fun x -> x) "qr"           qr          ;
    R.opt (fun x -> x) "singular.ok"  singular_ok ;
    R.opt (fun x -> x) "contrasts"    contrasts   ;
    R.opt (fun x -> x) "offset"       offset      ]

