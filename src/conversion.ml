(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2010 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*             guillaume.yziquel@citycable.ch                                    *)
(*********************************************************************************)

let rec list_of_pairlist (ll : 'a internallist) =
  match sexptype (ll : 'a internallist :> sexp) with
  | NilSxp -> [] | ListSxp | LangSxp | DotSxp ->
  (* There's a typing issue with the DotSxp sexptype... *)
  let ll = ((ll : 'a internallist :> sexp) : sexp :> 'a listsxp) in
  ( ((inspect_listsxp_tagval ll) : sexp :> symsxp (* This may be excessive *)), (inspect_listsxp_carval ll))
  :: (list_of_pairlist ((inspect_listsxp_cdrval ll) : sexp :> pairlist))
  | _ -> failwith "Conversion failure in list_of_listsxp."

let pairlist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in List.iter
  begin function (tag, value) ->
    let () = write_listsxp_element ((!cursor : pairlist :> sexp) : sexp :> pairlistsxp) tag value in
    cursor := ((inspect_listsxp_cdrval ((!cursor : pairlist :> sexp) : sexp :> pairlistsxp)) : sexp :> pairlist)
  end l; r_l

external cons : sexp -> pairlist -> pairlistsxp = "ocamlr_cons"
external tag : pairlistsxp -> string -> unit = "ocamlr_tag"
external set_langsxp : pairlistsxp -> unit = "ocamlr_set_langsxp"

let langsxp (f: sexp) (args: (string option * sexp) list) : langsxp =
  let lcons hd tl = let x = cons hd tl in set_langsxp x; ((x : pairlistsxp :> sexp) : sexp :> langsxp) in
  lcons f begin List.fold_right begin fun (t, hd) tl ->
    let x = cons hd tl in match t with
    | None -> ((x : pairlistsxp :> sexp) : sexp :> pairlist)
    | Some name -> tag x name; ((x : pairlistsxp :> sexp) : sexp :> pairlist)
  end args ((null_creator ()) : nilsxp :> pairlist) end

external string_of_charsxp : charvecsxp -> string = "ocamlr_internal_string_of_charsxp"

let list_of_vecsxp (access : 'a vecsxp -> int -> 'b) (s : 'a vecsxp) =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    let x = access s (lngth - n) in x::(aux (n - 1) s)
  in aux lngth s

let vecsxp_of_list (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'b -> unit) (l: 'b list) =
  let s = alloc (List.length l) in
  let rec aux offset = function | [] -> () | hd::tl ->
    let () = assign s offset hd in aux (1 + offset) tl
  in aux 0 l; s

let bool_list_of_lglvecsxp   = list_of_vecsxp access_lglvecsxp
let lglvecsxp_of_bool_list   = vecsxp_of_list alloc_lgl_vector assign_lglvecsxp
let bools_of_t tau = bool_list_of_lglvecsxp ((tau : bool list t :> sexp) : sexp :> lglvecsxp)
let bool_of_t tau = access_lglvecsxp ((tau : bool t :> sexp) : sexp :> lglvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the lgl vecsxp contains only one element. *)
let bool b = (((lglvecsxp_of_bool_list [b]) : lglvecsxp :> sexp) : sexp :> bool t)
let bools bl = (((lglvecsxp_of_bool_list bl) : lglvecsxp :> sexp) : sexp :> bool list t)

let int_list_of_intvecsxp    = list_of_vecsxp access_intvecsxp
let intvecsxp_of_int_list    = vecsxp_of_list alloc_int_vector assign_intvecsxp
let ints_of_t tau = int_list_of_intvecsxp ((tau : int list t :> sexp) : sexp :> intvecsxp)
let int_of_t tau = access_intvecsxp ((tau : int t :> sexp) : sexp :> intvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the int vecsxp contains only one element. *)
let int i = (((intvecsxp_of_int_list [i]) : intvecsxp :> sexp) : sexp :> int t)
let ints il = (((intvecsxp_of_int_list il) : intvecsxp :> sexp) : sexp :> int list t)

let float_list_of_realvecsxp = list_of_vecsxp access_realvecsxp
let realvecsxp_of_float_list = vecsxp_of_list alloc_real_vector assign_realvecsxp
let floats_of_t tau = float_list_of_realvecsxp ((tau : float list t :> sexp) : sexp :> realvecsxp)
let float_of_t tau = access_realvecsxp ((tau : float t :> sexp) : sexp :> realvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the real vecsxp contains only one element. *)
let float x = (((realvecsxp_of_float_list [x]) : realvecsxp :> sexp) : sexp :> float t)
let floats xl = (((realvecsxp_of_float_list xl) : realvecsxp :> sexp) : sexp :> float list t)

let string_list_of_strvecsxp = list_of_vecsxp access_strvecsxp
let strvecsxp_of_string_list = vecsxp_of_list alloc_str_vector assign_strvecsxp
let strings_of_t tau = string_list_of_strvecsxp ((tau : string list t :> sexp) : sexp :> strvecsxp)
let string_of_t tau = access_strvecsxp ((tau : string t :> sexp) : sexp :> strvecsxp) 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)
external string : string -> string t = "ocamlr_strsxp_of_string"
let strings sl = (((strvecsxp_of_string_list sl) : strvecsxp :> sexp) : sexp :> string list t)

let sexp_list_of_rawvecsxp = list_of_vecsxp access_rawvecsxp
let sexps_of_t tau = sexp_list_of_rawvecsxp ((tau : sexp list t :> sexp) : sexp :> rawvecsxp)

let langsxp_list_of_exprvecsxp = list_of_vecsxp access_exprvecsxp
let langsxps_of_t tau = langsxp_list_of_exprvecsxp ((tau : langsxp list t :> sexp) : sexp :> exprvecsxp)

