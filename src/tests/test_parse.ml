let _ = R.init();;
let prerr_error = function
| R.Raw.Parse_incomplete s ->
    prerr_endline (Printf.sprintf "Parse incomplete: %s" s)
| R.Raw.Parse_error s ->
    prerr_endline (Printf.sprintf "Parse error: %s" s)
| e ->
    prerr_endline (Printexc.to_string e)
;;
open R.Raw;;
let try_parse s =
  try
    let print = R.Raw.eval_string "print" in
    let sexp = R.Raw.parse_sexp s in
    let typ = R.Raw.sexptype sexp in
    let s_typ =
      match typ with
      | NilSxp -> "NilSxp"
      | SymSxp -> "SymSxp"
      | ListSxp -> "ListSxp"
      | ClosSxp -> "ClosSxp"
      | EnvSxp -> "EnvSxp"
      | PromSxp -> "PromSxp"
      | LangSxp -> "LangSxp"
      | SpecialSxp -> "SpecialSxp"
      | BuiltinSxp -> "BuiltinSxp"
      | CharSxp -> "CharSxp"
      | LglSxp -> "LglSxp"
      | IntSxp -> "IntSxp"
      | RealSxp -> "RealSxp"
      | CplxSxp -> "CplxSxp"
      | StrSxp -> "StrSxp"
      | DotSxp -> "DotSxp"
      | AnySxp -> "AnySxp"
      | VecSxp -> "VecSxp"
      | ExprSxp -> "ExprSxp"
      | BcodeSxp -> "BcodeSxp"
      | ExtptrSxp -> "ExtptrSxp"
      | WeakrefSxp -> "WeakrefSxp"
      | RawSxp -> "RawSxp"
      | S4Sxp -> "Sxp"
      | FunSxp -> "FunSxp"
    in
    prerr_endline s_typ;
    ignore(R.eval [print; sexp])
  with e -> prerr_error e
;;
try_parse "1:10";;
try_parse "x=1:10";;
try_parse "foo(";;
try_parse "!)";;
try_parse "[coucou)";;