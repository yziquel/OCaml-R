/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008-2009 Institut National de Recherche en                  */
/*    Informatique et en Automatique. All rights reserved.                       */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 3 of the         */
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

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <Rinternals.h>
#include <R.h>
#include <Rembedded.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>
#include <stdio.h>

/* Base functions. */

CAMLprim value init_r (value argv) {
  CAMLparam1(argv);
  int length = Wosize_val(argv);
  char* argv2[length];
  int i;
  for (i=0 ; i<length ; i++) {
    argv2[i]=String_val(Field(argv,i));
  }
  i = Rf_initEmbeddedR(length, argv2);
  CAMLreturn(Val_int(i));
}

CAMLprim void end_r () {
  CAMLparam0();
  Rf_endEmbeddedR(0);
  CAMLreturn0;
}

CAMLprim value r_sexp_of_symbol (value symbol) {
  CAMLparam1(symbol);
  char* c_symbol = String_val(symbol);
  SEXP e;

  prerr_endline (c_symbol); /* DEBUG */
  PROTECT(e = duplicate(findVar(install(c_symbol), R_GlobalEnv)));
  PrintValue(e); /* DEBUG */
  UNPROTECT(1);

  CAMLreturn(Val_sexp(e));
}

CAMLprim value r_sexp_of_string (value expression) {
  CAMLparam1(expression);
  char* c_name = "camlrtmp";
  char* s_exp;
  CAMLlocal1(result);
  SEXP e, tmp;
  int hadError;
  ParseStatus status;

  asprintf(&s_exp, "%s = %s", c_name, String_val(expression));
  PROTECT(tmp = mkString(s_exp));
  PROTECT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
  PrintValue(e); /* DEBUG */
  R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
  UNPROTECT(2);
  free(s_exp);
  result = r_sexp_of_symbol(caml_copy_string(c_name));
  CAMLreturn(result);
}


CAMLprim void r_set_var (value name, value sexp) {
  CAMLparam2(name,sexp);
  char* c_name = String_val(name);
  SEXP e = (SEXP) Long_val(Field(sexp,0));
  setVar(install(c_name), e, R_GlobalEnv);
  CAMLreturn0;
}

CAMLprim void r_print_value (value sexp) {
  CAMLparam1(sexp);
  SEXP e = (SEXP) Long_val(Field(sexp,0));
  PrintValue(e);
  CAMLreturn0;
}

CAMLprim value r_eval_langsxp (value sexp_list) {
  CAMLparam1(sexp_list);

  SEXP sexp2eval = (SEXP) Long_val(Field(sexp_list,0));
  SEXP e;
  int error = 0;

  /* Should this be wrapped with a PROTECT() and
     an UNPROTECT(1), or not? */
  PROTECT(e = R_tryEval(sexp2eval, R_GlobalEnv, &error));
  UNPROTECT(1);

  if (error) {caml_failwith(
    "OCaml-R error in eval_sexp_list C stub."
  );};

  CAMLreturn(Val_sexp(e));
}

CAMLprim void r_exec (value fun_name, value args) {
  CAMLparam2(fun_name, args);
  CAMLlocal2(tmpval,tmpval2);
  int nb_args = Wosize_val(args);
  int i;
  SEXP s, t, u;
  int error = 0;
  char* error_msg;
/*  prerr_endline (asprintf("%d args", nb_args));*/

  PROTECT(t = s = allocList(nb_args + 1));
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install(String_val(fun_name)));
  for (i = 0; i < nb_args; i++) {
    t = CDR(t);
    tmpval = Field(args,i);
    if (Is_block(tmpval)) {
      tmpval2 = Field(tmpval,1);
      if (Field(tmpval,0) == hash_variant("Named")) {
/*
        prerr_endline("Named");
        prerr_endline(String_val(Field(tmpval2,0)));
*/
        SET_TAG(t, install(String_val(Field(tmpval2,0))));
        u = Long_val(Field(Field(tmpval2,1),0));
/*
        PrintValue(u);
*/
        SETCAR(t, duplicate(u));
/*
        prerr_endline("Named ok");
*/
      } else if (Field(tmpval,0) == hash_variant("Anon")) {
/*
        prerr_endline ("Anon");
*/
        u = Long_val(Field(tmpval2,0));
/*
        PrintValue(u);
*/
        SETCAR(t, duplicate(u));
/*
        prerr_endline ("Anon ok");
*/
      } else {
        prerr_endline("bad constructor");
        /* bad constructor */
      }

    }  else
    {
      /* no argument, bad value for us */
      prerr_endline("no block, bad value");
    }
  }
  R_tryEval(s, R_GlobalEnv, &error);
  UNPROTECT(1);
  if (error) {
    asprintf(&error_msg, "R failure while calling function '%s'.", String_val(fun_name));
    caml_failwith(error_msg);
  }
  CAMLreturn0;
}

