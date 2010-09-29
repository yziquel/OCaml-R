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

(* Legacy, commented code.

(** {2 Casting and subtyping to low-level.} *)

type 'a t = private sexp
(**  This is the type of a wrapped R value whose semantics
  *  is the corresponding ['a] type in Objective Caml.
  *)

val cast : sexp -> 'a t
(**  This unsafe function allows us to cast a given [R.sexp]
  *  value to a semantically typed ['a R.t] value. This is
  *  a useful function, as R is a dynamically typed language.
  *)

*)


(** {2 The type system of OCaml-R.} *)

type untyped

type -'a typed = private untyped
type -'a sexptyped = private untyped constraint 'a =
  [< `Nil
  |  `Sym
  |  `List of [< `Pair | `Call ]
  |  `Clo
  |  `Env
  |  `Prom
  |  `Special
  |  `Builtin
  |  `Vec of [< `Char | `Lgl | `Int | `Real | `Str | `Raw | `Expr ]
  ]

type -'typing obj = private sexp
and +'a t = 'a typed obj
and +'a sxp = 'a sexptyped obj
and sexp = private untyped obj


(** {2 Types aliases. } *)

type nilsxp         = [`Nil]                                      sxp
type symsxp         = [`Sym]                                      sxp
type 'a listsxp     = [`List of [< `Pair | `Call ] as 'a]         sxp
and 'a internallist = [`Nil | `List of [< `Pair | `Call] as 'a]   sxp
type langsxp        = [`List of [`Call]]                          sxp
type closxp         = [`Clo]                                      sxp
type envsxp         = [`Env]                                      sxp
type promsxp        = [`Prom]                                     sxp
type builtinsxp     = [`Builtin]                                  sxp
type charvecsxp     = [`Vec  of [`Char]]                          sxp
type lglvecsxp      = [`Vec  of [`Lgl ]]                          sxp
type intvecsxp      = [`Vec  of [`Int ]]                          sxp
type realvecsxp     = [`Vec  of [`Real]]                          sxp
type strvecsxp      = [`Vec  of [`Str ]]                          sxp
type rawvecsxp      = [`Vec  of [`Raw ]]                          sxp
type exprvecsxp     = [`Vec  of [`Raw ]]                          sxp

