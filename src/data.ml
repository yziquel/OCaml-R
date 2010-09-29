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

(* Legacy, commented, code. *)

(*type 'a t = sexp

let cast : sexp -> 'a t = Obj.magic

type 'a promise = 'a Lazy.t t*)


(* The type system of OCaml-R. *)

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


(* Type aliases. *)

type nilsxp         = [`Nil]                                      sxp
type symsxp         = [`Sym]                                      sxp
type 'a listsxp     = [`List of [< `Pair | `Call ] as 'a]         sxp
and 'a internallist = [`Nil | `List of [< `Pair | `Call ] as 'a ] sxp
type langsxp        = [`List of [`Call]]                          sxp
type pairlistsxp    = [`List of [`Pair]]                          sxp
and pairlist        = [`Nil | `List of [`Pair]]                   sxp
type closxp         = [`Clo]                                      sxp
type envsxp         = [`Env]                                      sxp
type promsxp        = [`Prom]                                     sxp
type specialsxp     = [`Special]                                  sxp
type builtinsxp     = [`Builtin]                                  sxp
type 'a vecsxp      = [`Vec  of
    [< `Char | `Lgl | `Int  | `Real
    | `Str  | `Raw | `Expr ] as 'a
  ] sxp
type charvecsxp  = [`Vec  of [`Char]]                             sxp
type lglvecsxp   = [`Vec  of [`Lgl ]]                             sxp
type intvecsxp   = [`Vec  of [`Int ]]                             sxp
type realvecsxp  = [`Vec  of [`Real]]                             sxp
type strvecsxp   = [`Vec  of [`Str ]]                             sxp
type rawvecsxp   = [`Vec  of [`Raw ]]                             sxp
type exprvecsxp  = [`Vec  of [`Raw ]]                             sxp

