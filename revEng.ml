open Sexptype
open Sexprec
open Symbols
open Conversion
open Read_internal

(* This module is an ugly work in progress. It is supposed to be ugly... *)

(* Ugly memory allocation and management functions. *)
external sexp_allocate : unit -> sexp = "r_sexp_allocate"
external init_ocaml_node : unit -> unit = "r_init_ocaml_node"
external write_promise : prom sxp -> sexp -> unit = "r_write_promise"

(* Convenience declarations *)
let fun_of_call        : lang sxp -> sexp         = inspect_listsxp_carval
let args_of_call       : lang sxp -> pairlist sxp = inspect_listsxp_cdrval
let car_of_pairlist    : pairlist sxp -> sexp     = inspect_listsxp_carval
let cdr_of_pairlist    : pairlist sxp -> sexp     = inspect_listsxp_cdrval
let tag_of_pairlist    : pairlist sxp -> sexp     = inspect_listsxp_tagval
let formals_of_closure : clos sxp -> pairlist sxp = inspect_closxp_formals
let body_of_closure    : clos sxp -> pairlist sxp = inspect_closxp_body


(* Now the real work: *)

let mkPROMISE (expression : sexp) =
  let s = sexp_allocate () in
  let () = write_promise s expression in
  s

let rec promiseArgs (args : pairlist sxp) =
  (* promiseArgs is called by eval on the list of arguments of the call. *)
  let phi (tag, car) = match sexp_equality car (dots_symbol_creator ()) with
    | true -> let h = findvar car in
              begin match sexptype h with
              | NilSxp | DotSxp -> List.map
                begin function (tag, car) -> (tag, (mkPROMISE car)) end
                (list_of_lisplist h)
              | _ -> begin match sexp_equality h (missing_arg_creator ()) with
                     | true -> [] | false -> failwith "promiseArgs" end
              end
    | false -> begin match sexp_equality car (missing_arg_creator ()) with
    | true  -> (tag, (missing_arg_creator ()))::[]
    | false -> (tag, (mkPROMISE car))::[] end
  in lisplist_of_list (List.flatten (List.map phi (list_of_lisplist args)))

let matchArgs formals supplied call =

  (* matchArgs is coded in match.c. An informal description of matching
     provided in section 4.3.2. 'Argument matching' of the 'R Language
     Definition'. *)

  (* For now, we do not provide support for DotSxp. *)

  assert (ListSxp = sexptype formals);
  assert (ListSxp = sexptype supplied);

  (* We first do some exact matching of tags. *)
  let supplied_listsxp = list_of_lisplist supplied in
  let get_matching (tag, _) = List.filter
    begin function (tag_supplied, car_supplied) ->
      if (NilSxp = sexptype car_supplied) then false else
      pmatch tag tag_supplied true
    end supplied_listsxp in
  let aux = List.map get_matching (list_of_lisplist formals) in
  let exact_matched = List.map begin function
    | [] -> None | [x] -> Some x | _ -> failwith
    "formal argument matched by multiple actual arguments"
    end aux in
  (* We leave out for now the check that the an actual argument
     matched multiple formal arguments... *)

  (* We drop partial matching of arguments here. It is utterly
     overkill for simple reverse engineering work. *)

  (* We now implement the positional matching algorithm. *)
  let really_matched = List.map begin function
    | None -> assert false | Some x -> x end
    begin List.filter begin function
    | None -> false | _ -> true end
    exact_matched end in
  let really_unmatched = List.filter
    begin function x -> not (List.memq x really_matched) end
    supplied_listsxp
  let rec aux unmatched exct_match =
    match (unmatched, exct_match) with
    | _, ((Some x)::rest)    -> x::(aux unmatched rest)
    | (hd::tl), (None::rest) -> hd::(aux tl rest)
    | [], []                 -> []
    | [], (None::rest)       -> failwith
    "Could not match all formal arguments" in
  aux really_unmatched exact_matched

external apply_closure : lang sxp -> clos sxp -> pairlist sxp -> sexp = "r_apply_closure"

(*let ml_apply_closure call (op : clos sxp) arglist =
  assert (CloSxp = sexptype op);
  let formals = formals_of_closure op in
  let body = body_of_closure op in
  let actuals = matchArgs formals arglist call*)

let ml_eval (call : lang sxp) =                                               (* call is called e in eval.c *)
  (* In eval.c, there are dynamic type checks and different behaviours
     depending on whether the functions are symbols or else... We ajust
     it to our own use case: it's a symbol. *)
  let (f : clos sxp) = findfun (fun_of_call call) in                           (* f is called op in eval.c *)
  (* Subsequently, we'll be assuming that f is a CLOSXP, conforming to
     our use case. *)
  let args : pairlist sxp = args_of_call call in
  let prom_args : pairlist sxp = promiseArgs args in
  apply_closure call f prom_args

