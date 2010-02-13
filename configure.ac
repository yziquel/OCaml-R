#################################################################################
#                OCaml-R                                                        #
#                                                                               #
#    Copyright (C) 2008-2010 Institut National de Recherche en                  #
#    Informatique et en Automatique. All rights reserved.                       #
#                                                                               #
#    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation; either version 3 of the         #
#    License, or  any later version.                                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#             guillaume.yziquel@citycable.ch                                    #
#################################################################################

AC_INIT([ocaml-r], [0.2])
#AC_CONFIG_MACRO_DIR([m4])
m4_include([m4/ocaml.m4])

AC_PROG_CC
ROOT=`pwd`
AC_SUBST(ROOT)

AC_PATH_PROG(R,R,no)
if test "$R" = no ; then
        AC_MSG_ERROR(Cannot find R.)
fi

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler.])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG([unix])
if test "$OCAML_PKG_unix" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'unix'.])
fi

AC_CHECK_OCAML_PKG([calendar])
if test "$OCAML_PKG_calendar" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'calendar'.])
fi

AC_CONFIG_FILES([master.Makefile])
AC_OUTPUT
