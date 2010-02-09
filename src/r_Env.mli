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

(** {2 Declaration of environment - Initialisation.} *)

module type Environment = sig
(**  [Environment] is the type of a module containing all necessary
  *  informations and data in order to set up the R interpreter
  *  properly.
  *
  *  We still have to find a mechanism to strip down the R packages
  *  loaded at startup. *)

  val name : string
  (**  This is the [name] of the first argument of [argv] for R.
    *  Mandatory, otherwise [libR.so] segfaults immediately. *)

  val options : string list
  (**  Other command line options passed to the [libR.so] library when
    *  initialising R.
    *
    *  @see "R reference manual" File refman.pdf, page 452, section intitled
    *  'Startup - Initialization at Start of an R Session' for details
    *  about the most important command line options.
    *  @see <http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R>
    *  For command line options.
    *)

  val signal_handlers : bool
  (**  If set to [false], asks R not to install its signal handlers. I've
    *  been experiencing weird issues with R signal handlers, since, for
    *  instance, a [SIGSEGV] is sometimes caught by [libR.so], and R asks then
    *  asks whether or not you want to save your workspace, et ceteræ... By
    *  default, set to false.
    *)

  val env : (string * string) list
  (**  These are environment variables that needs to be set before
    *  initialising the R interpreter. In the [Standard] module, these
    *  values are determined when the binding itself is compiled.
    *)

  val packages : string list option
  (**  Packages loaded on startup. If set to [None], load the usual standard
    *  library. Otherwise, if set to [Some p], load packages [p] in place of
    *  the standard library. In the [Standard] module, this is set to [Some []]. *)

end

