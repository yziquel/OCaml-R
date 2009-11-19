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

#define _GNU_SOURCE   /* Required by asprinft (see 'man asprintf'), as it is a
                         GNU extension. Seems a bit overkill. Should try to
                         remove this dependency on asprintf. */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rinterface.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <stdio.h>


/* Stub code initialising and terminating the R interpreter. */

CAMLprim value init_r (value argv, value sigs) {

  /* -1- argv is an OCaml array of strings, which gives the command line
     arguments used to invoke the R interpreter. Code segfaults if
     the array does not contain a first element, which is the name of
     the program, typically "R", or "OCaml-R". Other arguments typically
     are "--vanilla", "--slave"...

     -2- sigs is an OCaml int. When set to 0, R signal handlers are not
     removed. When set to, say, 1, R signal handlers are removed. It is
     very useful to remove signal handlers when embedding R. */

  CAMLparam2(argv, sigs);
  int length = Wosize_val(argv);
  char* argv2[length];
  int i;

  // We duplicate the OCaml array into a C array.
  for (i=0; i<length; i++) argv2[i]=String_val(Field(argv, i));

  /* Don't let R set up its own signal handlers when sigs = 1.
     This requires R >= 2.3.1. */
  if (Int_val(sigs)) R_SignalHandlers = 0;

  // This is the libR.so function.
  i = Rf_initEmbeddedR(length, argv2);

  // Returns 1 if R is correctly initialised.
  CAMLreturn(Val_int(i));
}

CAMLprim value end_r (value unit) {
  /* This function terminates the R interpreter. It is not clear whether
     or not this function is garbage-collector-friendly. For details, see
     http://old.nabble.com/Reset-an-embedded-R.dll-td17236931.html */
  CAMLparam1(unit);
  Rf_endEmbeddedR(0);
  CAMLreturn(Val_unit);
}


/* Wrapping and unwrapping of R values. */

CAMLprim value Val_sexp (SEXP sexp) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value) sexp;
    /* Do not use Val_long in the above statement,
       as it will drop the top bit. See mlvalues.h. */
  CAMLreturn(result);
}

SEXP Sexp_val (value sexp) {
  return (SEXP) Field(sexp, 0);
}


/* The NULL constant in R... */

CAMLprim value r_null (value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_sexp(R_NilValue));
}


/* Extracting runtime R low-level type information. */

CAMLprim value r_sexptype_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(TYPEOF(Sexp_val(sexp))));
}


/* Beta-reduction in R. */

/* r_langsxp_of_list creates a lisp-like list out of an
   OCaml list. langsxps, called pairlists, are not much
   used in R these days, except for internal matters,
   such as being an argument to R_tryEval.
   - Argument l is the OCaml list.
   - Argument n is the length of the OCaml list. */
CAMLprim value r_langsxp_of_list (value l, value n) {
  CAMLparam2(l, n);
  CAMLlocal1(l_cursor);
  SEXP s, t;

  PROTECT(t = s = allocList(Int_val(n)));
  SET_TYPEOF(s, LANGSXP);

  /* Fill in list s, with t moving over the pairlist,
     with values from l */
  int first_time = 1;
  l_cursor = l;
  while (l_cursor && Is_block(l_cursor)) {
    if (first_time) {first_time = 0;} else {t = CDR(t);}
    SETCAR(t, Sexp_val(Field(l_cursor, 0)));
    l_cursor = Field(l_cursor, 1);
  }
  UNPROTECT(1);

  CAMLreturn(Val_sexp(s));
}

CAMLprim value r_eval_langsxp (value sexp_list) {
  /* sexp_list is an OCaml value containing a SEXP of sexptype LANGSXP.
     This is a LISP-style pairlist of SEXP values. r_eval_langsxp
     executes the whole string, and sends back the resulting SEXP wrapped
     up in an OCaml value. There's also an error handling mechanism. */
  CAMLparam1(sexp_list);

  SEXP e;        // Placeholder for the result of beta-reduction.
  int error = 0; // Error catcher boolean.

  /* Should this be wrapped with a PROTECT() and
     an UNPROTECT(1), or not? */
  PROTECT(e = R_tryEval(Sexp_val(sexp_list), R_GlobalEnv, &error));
  UNPROTECT(1);

  if (error) caml_failwith("OCaml-R error in eval_sexp_list C stub.");

  CAMLreturn(Val_sexp(e));
}



CAMLprim value r_sexp_of_symbol (value symbol) {
  /* Remark: explanation of the install function at
     https://stat.ethz.ch/pipermail/r-devel/2009-August/054494.html */
  CAMLparam1(symbol);
  char* c_symbol = String_val(symbol);
  SEXP e;

  /* prerr_endline (c_symbol); DEBUG */
  PROTECT(e = duplicate(findVar(install(c_symbol), R_GlobalEnv)));
  /* PrintValue(e); DEBUG */
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
  /* PrintValue(e); DEBUG */
  R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
  UNPROTECT(2);
  free(s_exp);
  result = r_sexp_of_symbol(caml_copy_string(c_name));
  CAMLreturn(result);
}

CAMLprim value r_set_var (value name, value sexp) {
  CAMLparam2(name,sexp);
  char* c_name = String_val(name);
  SEXP e = (SEXP) Long_val(Field(sexp,0));
  setVar(install(c_name), e, R_GlobalEnv);
  CAMLreturn(Val_unit);
}

CAMLprim value r_print_value (value sexp) {
  CAMLparam1(sexp);
  SEXP e = Sexp_val(sexp);
  PrintValue(e);
  CAMLreturn(Val_unit);
}

/* Commented out because of 'warning: assignment makes pointer from integer without a cast' */
//CAMLprim value r_exec (value fun_name, value args) {
//  CAMLparam2(fun_name, args);
//  CAMLlocal2(tmpval,tmpval2);
//  int nb_args = Wosize_val(args);
//  int i;
//  SEXP s, t, u;
//  int error = 0;
//  char* error_msg;
///*  prerr_endline (asprintf("%d args", nb_args));*/
//
//  PROTECT(t = s = allocList(nb_args + 1));
//  SET_TYPEOF(s, LANGSXP);
//  SETCAR(t, install(String_val(fun_name)));
//  for (i = 0; i < nb_args; i++) {
//    t = CDR(t);
//    tmpval = Field(args,i);
//    if (Is_block(tmpval)) {
//      tmpval2 = Field(tmpval,1);
//      if (Field(tmpval,0) == hash_variant("Named")) {
///*
//        prerr_endline("Named");
//        prerr_endline(String_val(Field(tmpval2,0)));
//*/
//        SET_TAG(t, install(String_val(Field(tmpval2,0))));
//        u = Long_val(Field(Field(tmpval2,1),0));
///*
//        PrintValue(u);
//*/
//        SETCAR(t, duplicate(u));
///*
//        prerr_endline("Named ok");
//*/
//      } else if (Field(tmpval,0) == hash_variant("Anon")) {
///*
//        prerr_endline ("Anon");
//*/
//        u = Long_val(Field(tmpval2,0));
///*
//        PrintValue(u);
//*/
//        SETCAR(t, duplicate(u));
///*
//        prerr_endline ("Anon ok");
//*/
//      } else {
//        prerr_endline("bad constructor");
//        /* bad constructor */
//      }
//
//    }  else
//    {
//      /* no argument, bad value for us */
//      prerr_endline("no block, bad value");
//    }
//  }
//  R_tryEval(s, R_GlobalEnv, &error);
//  UNPROTECT(1);
//  if (error) {
//    asprintf(&error_msg, "R failure while calling function '%s'.", String_val(fun_name));
//    caml_failwith(error_msg);
//  }
//  CAMLreturn(Val_unit);
//}


/* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs. */

CAMLprim value inspect_promsxp_value (value sexp) {
  CAMLparam1(sexp);
  struct promsxp_struct * s = (Sexp_val(sexp)).u;
  CAMLreturn(Val_sexp((*s).promsxp.value));
}

CAMLprim value inspect_promsxp_expr (value sexp) {
  CAMLparam1(sexp);
  struct promsxp_struct * s = (Sexp_val(sexp)).u;
  CAMLreturn(Val_sexp((*s).promsxp.expr));
}

CAMLprim value inspect_promsxp_env (value sexp) {
  CAMLparam1(sexp);
  struct promsxp_struct * s = (Sexp_val(sexp)).u;
  CAMLreturn(Val_sexp((*s).promsxp.env));
}
