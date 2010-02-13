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

module type Environment =
sig

  val name : string    (* This is the first argument of argv for R.
                          Mandatory, otherwise libR.so segfaults. *)

  (* See R reference manual, refman.pdf, page 452, section intitled
     'Startup - Initialization at Start of an R Session' for details
     about the most important command line options.
     More options are documented on the following webpage:
     http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R *)

  val options : string list

  (* signal_handlers, if set to false, asks R not to install its
     signal handlers. I've been experiencing weird issues with R signal
     handlers, since, for instance, a SIGSEGV originating from OCaml is
     caught by libR.so, and R asks then asks whether or not you want to
     save your workspace, et ceterae. By default, set to false. *)
  val signal_handlers : bool

  val env : (string * string) list

  val packages : string list option

end

