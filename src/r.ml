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


INCLUDE "environment.ml.p4"

module Standard : Environment = struct

  INCLUDE "standard.ml.p4"

end

INCLUDE "data.ml.p4"
INCLUDE "sexptype.ml.p4"
INCLUDE "sexprec.ml.p4"
INCLUDE "allocation.ml.p4"
INCLUDE "read_internal.ml.p4"
INCLUDE "write_internal.ml.p4"
INCLUDE "promise.ml.p4"
INCLUDE "symbols.ml.p4"
INCLUDE "conversion.ml.p4"
INCLUDE "internal.ml.p4"
INCLUDE "s3.ml.p4"
INCLUDE "s4.ml.p4"
INCLUDE "parser.ml.p4"
INCLUDE "reduction.ml.p4"
INCLUDE "initialisation.ml.p4"
