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

(** {2 S3 classes.} *)

(**  Virtual class for S3 objects in R. *)
class virtual s3 : object

  val virtual __underlying : sexp
  (**  Access to the underlying R data structure. *)

  method private attribute  : 'a. string -> 'a t
  (**  [attribute attr_name] returns the R data structure
    *  which is the object's attribute of name [attr_name].
    *  The typing of this method is deliberately unsafe, in
    *  order to allow the user to type things correctly. *)

  method attributes : (Specification.symbol * sexp) list
  (**  Returns the whole list of attributes of an S3 object. *)

  method classes    : string list
  (**  Returns the list of S3 classes that the object is
    *  an instance of. *)

end

class instance : object inherit s3 end

(**  Constructor of an [s3] object from an R S3 object. *)
val s3 : 'a t -> s3
