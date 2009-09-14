/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008 Institut National de Recherche en Informatique et       */
/*    en Automatique. All rights reserved.                                       */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 2 of the         */
/*    License, or  any later version.                                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

/* Access to the devices. */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <Rdefines.h>
#include <Rdevices.h>
#include "wrappers.h"

#define Val_devdesc(x) Val_long(x)
#define Devdesc_val(x) Long_val(x)

/*
CAMLprim void r_initGraphics() { InitGraphics(); }
CAMLprim void r_killAllDevices() { KillAllDevices(); }
CAMLprim void r_freeType1Fonts() { freeType1Fonts(); }
*/
