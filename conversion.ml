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
    let () = write_listsxp_element !cursor tag value in
    cursor := inspect_listsxp_cdrval !cursor
  end l; r_l

external cons : sexp -> sexp -> sexp = "ocamlr_cons"
external tag : sexp -> string -> unit = "ocamlr_tag"
external set_langsxp : sexp -> unit = "ocamlr_set_langsxp"

let langsxp (f: sexp) (args: (string option * sexp) list) : lang sxp =
  let lcons hd tl = let x = cons hd tl in set_langsxp x; x in
  lcons f begin List.fold_right begin fun (t, hd) tl ->
    let x = cons hd tl in match t with
    | None -> x | Some name -> tag x name; x
  end args (null_creator ()) end

external string_of_charsxp : vec_char sxp -> string = "ocamlr_internal_string_of_charsxp"

let list_of_vecsxp (access: 'a vecsxp -> int -> 'a) (s: 'a vecsxp) : 'a list =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    let x = access s (lngth - n) in x::(aux (n - 1) s)
  in aux lngth s

let vecsxp_of_list (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'a -> unit) (l: 'a list) : 'a vecsxp =
  let s = alloc (List.length l) in
  let rec aux offset = function | [] -> () | hd::tl ->
    let () = assign s offset hd in aux (1 + offset) tl
  in aux 0 l; s

let bool_list_of_lgl_vecsxp   = list_of_vecsxp access_lgl_vecsxp
let lgl_vecsxp_of_bool_list   = vecsxp_of_list alloc_lgl_vector assign_lgl_vecsxp
let bools_of_t : bool list t -> bool list = bool_list_of_lgl_vecsxp
let bool_of_t : bool t -> bool = fun t -> access_lgl_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the lgl vecsxp contains only one element. *)
let bool b = lgl_vecsxp_of_bool_list [b]
let bools = lgl_vecsxp_of_bool_list

let int_list_of_int_vecsxp    = list_of_vecsxp access_int_vecsxp
let int_vecsxp_of_int_list    = vecsxp_of_list alloc_int_vector assign_int_vecsxp
let ints_of_t : int list t -> int list = int_list_of_int_vecsxp
let int_of_t : int t -> int = fun t -> access_int_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the int vecsxp contains only one element. *)
let int i = int_vecsxp_of_int_list [i]
let ints = int_vecsxp_of_int_list

let float_list_of_real_vecsxp = list_of_vecsxp access_real_vecsxp
let real_vecsxp_of_float_list = vecsxp_of_list alloc_real_vector assign_real_vecsxp
let floats_of_t : float list t -> float list = float_list_of_real_vecsxp
let float_of_t : float t -> float = fun t -> access_real_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the real vecsxp contains only one element. *)
let float x = real_vecsxp_of_float_list [x]
let floats = real_vecsxp_of_float_list

let string_list_of_str_vecsxp = list_of_vecsxp access_str_vecsxp
let str_vecsxp_of_string_list = vecsxp_of_list alloc_str_vector assign_str_vecsxp
let strings_of_t : string list t -> string list = string_list_of_str_vecsxp
let string_of_t : string t -> string = fun t -> access_str_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)
external string : string -> string t = "ocamlr_strsxp_of_string"
let strings = str_vecsxp_of_string_list

let sexp_list_of_sexp_vecsxp = list_of_vecsxp access_sexp_vecsxp
let sexps_of_t : sexp list t -> sexp list = sexp_list_of_sexp_vecsxp

let lang_sxp_list_of_expr_vecsxp = list_of_vecsxp access_expr_vecsxp
let lang_sxps_of_t : lang sxp list t -> lang sxp list = lang_sxp_list_of_expr_vecsxp

