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

(**  Binding for the R interpreter. It encapsulates the
  *  functionalities of the [libR.so] shared library provided by the R
  *  software. This enables us to {b embed the R interpreter} into Objective
  *  Caml, to execute R code from Objective Caml and to exchange data
  *  structures between R and Objective Caml.
  *)

(**  {b THREAD SAFETY}
  *
  *  It is important to understand that this binding is a rather low-level
  *  binding of R functionality. As such, it is no more thread-safe than R
  *  itself, which is not thread-safe at all. Therefore, avoid real threading
  *  unless you know what you're doing...
  *)

(**  {b DATA CONVERSION}
  *
  *  R is an array-oriented language. Therefore, simple values such as
  *  a boolean, a string, a number, are in fact encapsulated, in R, in an
  *  array of booleans, an array of strings, an array of numbers. For a
  *  simple value, the array has only one element.
  *
  *  Moreover, as R is scientific software, it is important that data types
  *  be correctly matched between Objective Caml and R. At the moment, they
  *  are not. I am thinking here of the 31/32 bit issues, or 63/64 bit issue,
  *  or, for instance, of the fact that we are converting R arrays to
  *  Objective Caml lists, which isn't necessarily the smartest choice of
  *  data structures.
  *)

