let rec list_of_lisplist (ll : 'a lisplist sxp) =
  match sexptype ll with
  | NilSxp -> []   (* Typing will have to take into account that NULL is a list. *)
  | ListSxp | LangSxp | DotSxp ->  (* There's also a typing issue here... *)
  ( (inspect_listsxp_tagval ll), (inspect_listsxp_carval ll))
  :: (list_of_lisplist (inspect_listsxp_cdrval ll))
  | _ -> failwith "Conversion failure in list_of_lisplist."

let lisplist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in List.iter
  begin function (tag, value) ->
    let () = write_lisplist_element !cursor tag value in
    cursor := inspect_listsxp_cdrval !cursor
  end l; r_l

external langsxp_of_list : sexp list -> int -> lang sxp = "r_langsxp_of_list"

external string_of_charsxp : vec_char sxp -> string = "r_internal_string_of_charsxp"

let list_of_vecsxp (access: 'a vecsxp -> int -> 'a) (s: 'a vecsxp) : 'a list =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    (access s (lngth - n))::(aux (n - 1) s)
  in aux lngth s

let string_list_of_str_vecsxp = list_of_vecsxp access_str_vecsxp

let strings_of_t : string list t -> string list = string_list_of_str_vecsxp

let string_of_t : string t -> string = fun t -> access_str_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)

external string : string -> string t = "r_strsxp_of_string"

external access_int_vecsxp : vec_int sxp -> int -> int = "r_access_int_vecsxp"

let int_list_of_int_vecsxp = list_of_vecsxp access_int_vecsxp

