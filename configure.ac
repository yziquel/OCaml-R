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
