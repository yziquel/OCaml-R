(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2009 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Camlp4 extension to make usage of OCaml-R library easier. *)

open Camlp4.PreCast;;
open Syntax;;
open Ast;;

EXTEND Gram
  GLOBAL: expr str_item;

  expr: LEVEL ";" [
   [ "letR" ; id = LIDENT ; "=" ; code = STRING ; "in" ; e = expr ->
    <:expr<let $lid:id$ = R.sexp $str:code$ in $e$ >>
   ]
   | NONA
   [
    id = LIDENT ; "<--" ; e = expr ->
    <:expr<R.set_var $str:id$ $e$ >>
   ]
  ];
  str_item: LEVEL "top" [
   [
     "letR" ; id = LIDENT ; "=" ; code = STRING ->
      <:str_item<let $lid:id$ = R.sexp $str:code$ >>
   | "letR" ; id = LIDENT ; "=" ; code = STRING ; "in" ; e = expr ->
      <:str_item<let $lid:id$ = R.sexp $str:code$ in $e$ >>
   ]
   | NONA
     [ id = LIDENT ; "<--" ; e = expr ->
       <:str_item<R.set_var $str:id$ $e$ >>
     ]
  ];
  END

