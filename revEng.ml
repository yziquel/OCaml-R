(* This module is an ugly work in progress. It is supposed to be ugly...
   The purpose of this module is to investigate how to make proper function
   calls via the eval function of eval.c. *)

external unsafe_eval : lang sxp -> sexp = "r_reveng_eval_sxp"
external promise_args : pairlist sxp -> sexp = "r_reveng_promise_args"
external match_args : pairlist sxp -> pairlist sxp -> lang sxp -> sexp = "r_reveng_match_args"
external new_environment : sexp -> sexp -> sexp -> sexp = "r_reveng_new_environment"
external mkPROMISE : sexp -> sexp -> sexp = "r_reveng_mkPROMISE"
external set_missing : sexp -> int -> unit = "r_reveng_SET_MISSING"
external apply_closure : lang sxp -> clos sxp -> pairlist sxp -> sexp = "r_apply_closure"

let rec ml_apply_closure call closure arglist =
  let formals   = inspect_closxp_formals closure in
  let body      = inspect_closxp_body    closure in
  let saved_rho = inspect_closxp_env     closure in
  let actuals   = match_args formals arglist call in
  let new_rho   = new_environment formals actuals saved_rho in
  let actuals_cursor = ref actuals in
  List.iter begin function (_, value) ->
    if (sexp_equality (inspect_listsxp_carval !actuals_cursor) (missing_arg_creator ()))
    && (not (sexp_equality value (missing_arg_creator ())))
    then begin
      write_lisplist_carval !actuals_cursor (mkPROMISE value new_rho);
      set_missing !actuals_cursor 2
    end;
    actuals_cursor := inspect_lisplist_cdrval !actuals_cursor
  end (list_of_listsxp formals)

let rec ml_unsafe_eval call =
  print_endline "Entering ml_unsafe_eval."; 
  match sexptype call with
  | CloSxp -> null_creator ()
  | LangSxp ->
      let op = begin match sexptype (inspect_listsxp_carval call) with
               | SymSxp -> findfun (inspect_listsxp_carval call)
               | _      -> ml_unsafe_eval (inspect_listsxp_carval call)
               end in
      begin match sexptype op with
   (* | SpecialSxp -> *)
   (* | BuiltinSxp -> *)
      | CloSxp -> let pr_args = promise_args (inspect_listsxp_cdrval call) in
                  apply_closure call op pr_args
      | _ -> failwith ("Attempt to apply non-function. Sexptype: "^(string_of_sexptype (sexptype op))^".")
      end
  | _ -> failwith "Wrong sexptype for call."